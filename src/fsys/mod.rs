//! An Acme style filesystem interface for ad
//!
//!
//! ## Mount Point
//! https://www.pathname.com/fhs/pub/fhs-2.3.html#VARLIBLTEDITORGTEDITORBACKUPFILESAN
//!
//! `/var/lib/ad` feels like it would be the "correct" place to mount the filesystem but
//! this would need to be created with something like:
//! ```sh
//! $ sudo mkdir /var/lib/ad
//! $ sudo chown $(whoami) /var/lib/ad
//! ```
//!
//! So instead we mount to `$HOME/.ad/mnt/`
//!
//! ## Filesystem contents
//! ```text
//! $HOME/.ad/mnt/
//!   ctrl
//!   buffers/
//!     [n]/
//!       filename
//!       dot
//!       addr
//!       body
//!       event
//! ```
use crate::input::InputEvent;
use ninep::{
    fs::{FileMeta, IoUnit, Mode, Perm, Stat},
    server::{ReadOutcome, Serve9p, Server},
    Result,
};
use std::{
    env,
    fs::create_dir_all,
    sync::mpsc::{Receiver, Sender},
    thread::JoinHandle,
    time::SystemTime,
};
use tracing::trace;

mod buffer;
mod message;

pub(crate) use buffer::BufId;
pub(crate) use message::{Message, Req};

use buffer::BufferNodes;

const DEFAULT_SOCKET_NAME: &str = "ad";
const MOUNT_DIR: &str = ".ad/mnt";
const IO_UNIT: u32 = 8168;

// Fixed qids inside of '$HOME/.ad/mnt/buffers':
///   0. $HOME/.ad/mnt  -> The directory we mount to
const MOUNT_ROOT_QID: u64 = 0;
///   1.   /ctl         -> control file for issuing commands
const CONTROL_FILE_QID: u64 = 1;
const CONTROL_FILE: &str = "ctl";
///   2    /buffers/    -> parent directory for buffers
const BUFFERS_QID: u64 = 2;
const BUFFERS_DIR: &str = "buffers";
//    3      /index     -> a listing of all of the currently open buffers
const INDEX_BUFFER_QID: u64 = 3;
const INDEX_BUFFER: &str = "index";
//    4      /current   -> the fsys filename of the current buffer
const CURRENT_BUFFER_QID: u64 = 4;
const CURRENT_BUFFER: &str = "current";

/// The number of qids required to serve both the directory and contents
/// of a buffer node (used to generate qid values for buffers):
///
///   1. $id            -> The buffer directory
///   2.   filename     -> The current filename for the buffer
///   3.   dot          -> The text currently held in dot
///   4.   addr         -> The address value of dot
///   5.   xdot         -> The text currently held in xdot (a virtual dot not affecting real dot)
///   6.   xaddr        -> The address value of xdot
///   7.   body         -> The full body of the buffer
///   8.   event        -> Contol file for intercepting input events for the buffer
///   9.   output       -> Write only output connected to stdout/err of commands run within the buffer
const QID_OFFSET: u64 = 9;

const E_UNKNOWN_FILE: &str = "unknown file";

/// The filesystem interface for ad
#[derive(Debug)]
pub(crate) struct AdFs {
    tx: Sender<InputEvent>,
    buffer_nodes: BufferNodes,
    // Root level files and directories
    mount_dir_stat: Stat,
    control_file_stat: Stat,
}

/// A join handle for the filesystem thread
#[derive(Debug)]
pub struct FsHandle(JoinHandle<()>);

impl FsHandle {
    /// Join on the filesystem thread
    pub fn join(self) {
        _ = self.0.join();
    }
}

impl AdFs {
    /// Construct a new filesystem interface using channels held by the editor.
    pub fn new(tx: Sender<InputEvent>, brx: Receiver<BufId>) -> Self {
        let home = env::var("HOME").expect("$HOME to be set");
        let mount_path = format!("{home}/{MOUNT_DIR}");

        create_dir_all(&mount_path).expect("to be able to create our mount point");
        let buffer_nodes = BufferNodes::new(tx.clone(), brx);

        Self {
            tx,
            buffer_nodes,
            mount_dir_stat: empty_dir_stat(MOUNT_ROOT_QID, "/"),
            control_file_stat: empty_file_stat(CONTROL_FILE_QID, CONTROL_FILE),
        }
    }

    /// Spawn a thread for running this filesystem and return a handle to it
    pub fn run_threaded(self) -> FsHandle {
        let s = Server::new(self);
        FsHandle(s.serve_socket(DEFAULT_SOCKET_NAME.to_string()))
    }

    fn is_known_qid(&self, qid: u64) -> bool {
        [
            MOUNT_ROOT_QID,
            CONTROL_FILE_QID,
            BUFFERS_QID,
            INDEX_BUFFER_QID,
            CURRENT_BUFFER_QID,
        ]
        .contains(&qid)
            || self.buffer_nodes.is_known_buffer_qid(qid)
    }
}

impl Serve9p for AdFs {
    fn stat(&mut self, qid: u64, uname: &str) -> Result<Stat> {
        trace!(%qid, %uname, "handling stat request");
        self.buffer_nodes.update();

        match qid {
            MOUNT_ROOT_QID => Ok(self.mount_dir_stat.clone()),
            CONTROL_FILE_QID => Ok(self.control_file_stat.clone()),
            BUFFERS_QID => Ok(self.buffer_nodes.stat().clone()),
            qid => match self.buffer_nodes.get_stat_for_qid(qid) {
                Some(stat) => Ok(stat.clone()),
                None => Err(E_UNKNOWN_FILE.to_string()),
            },
        }
    }

    fn write_stat(&mut self, qid: u64, stat: Stat, uname: &str) -> Result<()> {
        trace!(%qid, %uname, "handling write stat request");
        self.buffer_nodes.update();

        if stat.n_bytes == 0 {
            trace!(%qid, %uname, "stat n_bytes=0, truncating file");
            match qid {
                MOUNT_ROOT_QID => self.mount_dir_stat.n_bytes = 0,
                CONTROL_FILE_QID => self.control_file_stat.n_bytes = 0,
                qid => self.buffer_nodes.truncate(qid),
            }
        }

        Ok(())
    }

    fn walk(&mut self, parent_qid: u64, child: &str, uname: &str) -> Result<FileMeta> {
        trace!(%parent_qid, %child, %uname, "handling walk request");
        self.buffer_nodes.update();

        match parent_qid {
            MOUNT_ROOT_QID => match child {
                CONTROL_FILE => Ok(self.control_file_stat.fm.clone()),
                BUFFERS_DIR => Ok(self.buffer_nodes.stat().fm.clone()),
                _ => match self.buffer_nodes.lookup_file_stat(parent_qid, child) {
                    Some(stat) => Ok(stat.fm.clone()),
                    None => Err(format!("{E_UNKNOWN_FILE}: {parent_qid} {child}")),
                },
            },

            qid if qid == BUFFERS_QID || self.buffer_nodes.is_known_buffer_qid(qid) => {
                match self.buffer_nodes.lookup_file_stat(qid, child) {
                    Some(stat) => Ok(stat.fm.clone()),
                    None => Err(format!("{E_UNKNOWN_FILE}: {parent_qid} {child}")),
                }
            }

            _ => Err(format!("{E_UNKNOWN_FILE}: {parent_qid} {child}")),
        }
    }

    fn open(&mut self, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit> {
        trace!(%qid, %uname, ?mode, "handling open request");
        self.buffer_nodes.update();

        if self.is_known_qid(qid) {
            Ok(IO_UNIT)
        } else {
            Err(format!("{E_UNKNOWN_FILE}: {qid}"))
        }
    }

    fn read(&mut self, qid: u64, offset: usize, count: usize, uname: &str) -> Result<ReadOutcome> {
        trace!(%qid, %offset, %count, %uname, "handling read request");
        self.buffer_nodes.update();

        let data = match qid {
            CONTROL_FILE_QID => Vec::new(),

            qid => match self.buffer_nodes.get_file_content(qid) {
                Some(content) => content
                    .as_bytes()
                    .iter()
                    .copied()
                    .skip(offset)
                    .take(count)
                    .collect(),

                None => return Err(format!("{E_UNKNOWN_FILE}: {qid}")),
            },
        };

        Ok(ReadOutcome::Immediate(data))
    }

    fn read_dir(&mut self, qid: u64, uname: &str) -> Result<Vec<Stat>> {
        trace!(%qid, %uname, "handling read dir request");
        self.buffer_nodes.update();

        match qid {
            MOUNT_ROOT_QID => Ok(vec![
                self.control_file_stat.clone(),
                self.buffer_nodes.stat().clone(),
            ]),
            BUFFERS_QID => Ok(self.buffer_nodes.top_level_stats()),
            qid => self
                .buffer_nodes
                .buffer_level_stats(qid)
                .ok_or_else(|| E_UNKNOWN_FILE.to_string()),
        }
    }

    fn write(&mut self, qid: u64, offset: usize, data: Vec<u8>, uname: &str) -> Result<usize> {
        trace!(%qid, %offset, n_bytes=%data.len(), %uname, "handling write request");
        self.buffer_nodes.update();

        let n_bytes = data.len();
        let s = match String::from_utf8(data.to_vec()) {
            Ok(s) => s,
            Err(e) => return Err(format!("Invalid data: {e}")),
        };

        match qid {
            CONTROL_FILE_QID => {
                self.control_file_stat.last_modified = SystemTime::now();
                match Message::send(Req::ControlMessage { msg: s }, &self.tx) {
                    Ok(_) => Ok(n_bytes),
                    Err(e) => Err(format!("unable to execute control message: {e}")),
                }
            }

            CURRENT_BUFFER_QID | INDEX_BUFFER_QID => Err(E_UNKNOWN_FILE.to_string()),

            qid => self.buffer_nodes.write(qid, s, offset),
        }
    }

    fn remove(&mut self, qid: u64, uname: &str) -> Result<()> {
        trace!(%qid, %uname, "handling remove request");
        // TODO: allow remove of a buffer to close the buffer
        Err("remove not allowed".to_string())
    }

    fn create(
        &mut self,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(FileMeta, IoUnit)> {
        trace!(%parent, %name, ?perm, ?mode, %uname, "handling create request");
        Err("create not allowed".to_string())
    }
}

fn empty_dir_stat(qid: u64, name: &str) -> Stat {
    Stat {
        fm: FileMeta::dir(name, qid),
        perms: Perm::OWNER_READ | Perm::OWNER_EXEC,
        n_bytes: 0,
        last_accesses: SystemTime::now(),
        last_modified: SystemTime::now(),
        owner: "ad".into(),
        group: "ad".into(),
        last_modified_by: "ad".into(),
    }
}

fn empty_file_stat(qid: u64, name: &str) -> Stat {
    Stat {
        fm: FileMeta::file(name, qid),
        perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
        n_bytes: 0,
        last_accesses: SystemTime::now(),
        last_modified: SystemTime::now(),
        owner: "ad".into(),
        group: "ad".into(),
        last_modified_by: "ad".into(),
    }
}
