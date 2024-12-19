//! Built-in minimal LSP support for ad
//!
//! See the LSP spec for details of semantics:
//!   https://microsoft.github.io/language-server-protocol/specification
use crate::{
    buffer::{Buffer, Buffers},
    config::{LangConfig, LspConfig},
    config_handle, die,
    editor::{Action, Actions, MbSelect, MbSelector, MiniBufferSelection, ViewPort},
    input::Event,
    lsp::{
        capabilities::{Capabilities, PositionEncoding},
        client::{LspClient, LspMessage},
        messages::{LspNotification, LspRequest, NotificationHandler, RequestHandler},
        rpc::{Message, Notification, Request, RequestId, Response},
    },
    util::ReadOnlyLock,
};
use lsp_types::{request::Initialize, NumberOrString, Uri};
use std::{
    collections::HashMap,
    sync::{
        mpsc::{channel, Receiver, Sender},
        Arc, RwLock,
    },
    thread::spawn,
};
use tracing::{debug, error, warn};

mod capabilities;
mod client;
mod messages;
mod rpc;

pub use capabilities::Coords;

const LSP_FILE: &str = "+lsp";

#[derive(Debug)]
pub(crate) enum Req {
    Start {
        lang: String,
        cmd: String,
        args: Vec<String>,
        root: String,
        open_bufs: Vec<PendingParams>,
    },
    Stop {
        lsp_id: usize,
    },
    Pending(PendingRequest),
    Message(LspMessage),
}

#[derive(Debug)]
pub struct LspManagerHandle {
    tx_req: Sender<Req>,
    capabilities: ReadOnlyLock<HashMap<String, (usize, Capabilities)>>,
    diagnostics: ReadOnlyLock<HashMap<Uri, Vec<Diagnostic>>>,
    configs: Vec<LangConfig>,
}

impl LspManagerHandle {
    #[cfg(test)]
    pub(crate) fn new_stubbed(tx_req: Sender<Req>) -> Self {
        Self {
            tx_req,
            capabilities: Default::default(),
            diagnostics: Default::default(),
            configs: Default::default(),
        }
    }

    #[inline]
    fn send(&self, lsp_id: usize, pending: PendingParams) {
        let req = Req::Pending(PendingRequest { lsp_id, pending });
        if let Err(e) = self.tx_req.send(req) {
            die!("LSP manager died: {e}")
        }
    }

    /// Will return None if there is no active client with recorded capabilities for the
    /// given language.
    fn lsp_id_and_encoding_for(&self, b: &Buffer) -> Option<(usize, PositionEncoding)> {
        let (lang, _) = &self.config_for_buffer(b)?;

        self.capabilities
            .read()
            .unwrap()
            .get(lang.as_str())
            .map(|(id, caps)| (*id, caps.position_encoding))
    }

    fn config_for_buffer(&self, b: &Buffer) -> Option<(&String, &LspConfig)> {
        let os_ext = b.path()?.extension()?;
        let ext = os_ext.to_str()?;
        self.configs
            .iter()
            .find(|c| c.extensions.iter().any(|e| e == ext))
            .and_then(|c| c.lsp.as_ref().map(|lsp| (&c.name, lsp)))
    }

    fn start_req_for_buf(&self, bs: &Buffers) -> Option<Req> {
        let b = bs.active();
        let (lang, config) = self.config_for_buffer(b)?;
        let root = config.root_for_buffer(b)?.to_str()?.to_owned();
        let open_bufs: Vec<_> = bs
            .iter()
            .flat_map(|b| match self.config_for_buffer(b) {
                Some((blang, _)) if blang == lang => Some(PendingParams::DocumentOpen {
                    lang: lang.to_owned(),
                    path: b.full_name().to_owned(),
                    content: b.str_contents(),
                }),
                _ => None,
            })
            .collect();

        Some(Req::Start {
            lang: lang.to_owned(),
            cmd: config.command.clone(),
            args: config.args.clone(),
            root,
            open_bufs,
        })
    }

    pub fn start_client(&self, bs: &Buffers) -> Option<&'static str> {
        match self.start_req_for_buf(bs) {
            Some(req) => {
                debug!("starting LSP server");
                if let Err(e) = self.tx_req.send(req) {
                    die!("LSP manager died: {e}")
                }
                None
            }

            None => Some("no LSP available for buffer"),
        }
    }

    pub fn stop_client(&self, b: &Buffer) {
        if let Some((lsp_id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("stopping LSP server {lsp_id}");
            if let Err(e) = self.tx_req.send(Req::Stop { lsp_id }) {
                die!("LSP manager died: {e}")
            }
        };
    }

    pub fn show_server_capabilities(&self, b: &Buffer) -> Option<(&'static str, String)> {
        let (lang, _) = &self.config_for_buffer(b)?;
        let txt = self
            .capabilities
            .read()
            .unwrap()
            .get(lang.as_str())?
            .1
            .as_pretty_json()?;

        Some((LSP_FILE, txt))
    }

    pub fn show_diagnostics(&self, b: &Buffer) -> Action {
        self.document_changed(b); // to ensure that diagnostics are up to date
        debug!("showing LSP diagnostics");
        let guard = self.diagnostics.read().unwrap();
        let mut diags: Vec<Diagnostic> = guard.values().flatten().cloned().collect();
        diags.sort_unstable();

        Action::MbSelect(Diagnostics(diags).into_selector())
    }

    pub fn document_opened(&self, b: &Buffer) {
        let lang = match self.config_for_buffer(b) {
            Some((lang, _)) => lang.clone(),
            None => return,
        };

        if let Some((id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didOpen ({id})");
            let path = b.full_name().to_string();
            let content = b.str_contents();

            self.send(
                id,
                PendingParams::DocumentOpen {
                    lang,
                    path,
                    content,
                },
            )
        }
    }

    pub fn document_closed(&self, b: &Buffer) {
        if let Some((id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didClose ({id})");
            let path = b.full_name().to_string();

            self.send(id, PendingParams::DocumentClose { path })
        }
    }

    pub fn document_changed(&self, b: &Buffer) {
        if let Some((id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didChange ({id})");
            let path = b.full_name().to_string();
            let content = b.str_contents();

            self.send(
                id,
                PendingParams::DocumentChange {
                    path,
                    content,
                    version: 2,
                },
            )
        }
    }

    pub fn goto_declaration(&self, b: &Buffer) {
        if let Some((id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }
            debug!("sending LSP textDocument/declaration ({id})");
            self.send(id, PendingParams::GotoDeclaration(enc.buffer_pos(b)))
        }
    }

    pub fn goto_definition(&self, b: &Buffer) {
        if let Some((id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }
            debug!("sending LSP textDocument/definition ({id})");
            self.send(id, PendingParams::GotoDefinition(enc.buffer_pos(b)))
        }
    }

    pub fn goto_type_definition(&self, b: &Buffer) {
        if let Some((id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }
            debug!("sending LSP textDocument/typeDefinition ({id})");
            self.send(id, PendingParams::GotoTypeDefinition(enc.buffer_pos(b)))
        }
    }

    pub fn hover(&self, b: &Buffer) {
        if let Some((id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }
            debug!("sending LSP textDocument/hover ({id})");
            self.send(id, PendingParams::Hover(enc.buffer_pos(b)))
        }
    }

    pub fn find_references(&self, b: &Buffer) {
        if let Some((id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }
            debug!("sending LSP textDocument/references ({id})");
            self.send(id, PendingParams::FindReferences(enc.buffer_pos(b)))
        }
    }
}

#[derive(Debug)]
pub struct LspManager {
    clients: HashMap<usize, LspClient>,
    // lang -> (lspID, server capabilities)
    capabilities: Arc<RwLock<HashMap<String, (usize, Capabilities)>>>,
    // (lspID, ReqID) -> in-flight requests we need a response for
    pending: HashMap<(usize, RequestId), Pending>,
    // lspID -> map of progress token -> title
    progress_tokens: HashMap<usize, HashMap<NumberOrString, String>>,
    diagnostics: Arc<RwLock<HashMap<Uri, Vec<Diagnostic>>>>,
    tx_req: Sender<Req>,
    tx_events: Sender<Event>,
    next_id: usize,
}

impl LspManager {
    pub fn spawn(tx_events: Sender<Event>) -> LspManagerHandle {
        let (tx_req, rx_req) = channel();
        let manager = Self {
            clients: Default::default(),
            capabilities: Default::default(),
            pending: Default::default(),
            progress_tokens: Default::default(),
            diagnostics: Default::default(),
            tx_req: tx_req.clone(),
            tx_events,
            next_id: 0,
        };

        let capabilities = ReadOnlyLock::new(manager.capabilities.clone());
        let diagnostics = ReadOnlyLock::new(manager.diagnostics.clone());
        spawn(move || manager.run(rx_req));

        LspManagerHandle {
            tx_req,
            capabilities,
            diagnostics,
            configs: config_handle!().languages.clone(),
        }
    }

    fn run(mut self, rx_req: Receiver<Req>) {
        for r in rx_req.into_iter() {
            match r {
                Req::Start {
                    lang,
                    cmd,
                    args,
                    root,
                    open_bufs,
                } => self.start_client(lang, cmd, args, root, open_bufs),
                Req::Stop { lsp_id } => self.stop_client(lsp_id),
                Req::Pending(p) => self.handle_pending(p),
                Req::Message(LspMessage { lsp_id, msg }) => match msg {
                    Message::Request(r) => self.handle_request(lsp_id, r),
                    Message::Response(r) => self.handle_response(lsp_id, r),
                    Message::Notification(n) => self.handle_notification(lsp_id, n),
                },
            }
        }
    }

    fn handle_pending(&mut self, PendingRequest { lsp_id, pending }: PendingRequest) {
        use lsp_types::{
            notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
            request::{
                GotoDeclaration, GotoDefinition, GotoTypeDefinition, HoverRequest, References,
            },
        };

        match pending {
            PendingParams::DocumentOpen {
                lang,
                path,
                content,
            } => DidOpenTextDocument::send(lsp_id, (lang, path, content), self),
            PendingParams::DocumentClose { path } => DidCloseTextDocument::send(lsp_id, path, self),
            PendingParams::DocumentChange {
                path,
                content,
                version,
            } => DidChangeTextDocument::send(lsp_id, (path, content, version as i32), self),
            PendingParams::GotoDeclaration(pos) => GotoDeclaration::send(lsp_id, pos, (), self),
            PendingParams::GotoDefinition(pos) => GotoDefinition::send(lsp_id, pos, (), self),
            PendingParams::GotoTypeDefinition(pos) => {
                GotoTypeDefinition::send(lsp_id, pos, (), self)
            }
            PendingParams::Hover(pos) => HoverRequest::send(lsp_id, pos, (), self),
            PendingParams::FindReferences(pos) => References::send(lsp_id, pos, (), self),
        }
    }

    fn handle_request(&mut self, lsp_id: usize, req: Request) {
        use lsp_types::request as req;

        RequestHandler {
            lsp_id,
            r: Some(req),
            man: self,
        }
        .handle::<req::WorkDoneProgressCreate>()
        .log_unhandled();
    }

    fn handle_response(&mut self, lsp_id: usize, res: Response) {
        use lsp_types::request as req;
        use Pending::*;

        let p = match self.pending.remove(&(lsp_id, res.id())) {
            Some(p) => p,
            None => {
                warn!("LSP - got response for unknown request: {res:?}");
                return;
            }
        };

        let actions = match p {
            FindReferences => req::References::handle(lsp_id, res, (), self),
            GotoDeclaration => req::GotoDeclaration::handle(lsp_id, res, (), self),
            GotoDefinition => req::GotoDefinition::handle(lsp_id, res, (), self),
            GotoTypeDefinition => req::GotoTypeDefinition::handle(lsp_id, res, (), self),
            Hover => req::HoverRequest::handle(lsp_id, res, (), self),
            Initialize(l, ob) => req::Initialize::handle(lsp_id, res, (l, ob), self),
        };

        if let Some(actions) = actions {
            if self.tx_events.send(Event::Actions(actions)).is_err() {
                error!("LSP - sender actions channel closed: exiting");
            }
        }
    }

    pub fn handle_notification(&mut self, lsp_id: usize, n: Notification) {
        use lsp_types::notification as notif;

        NotificationHandler {
            lsp_id,
            n: Some(n),
            man: self,
        }
        .handle::<notif::Progress>()
        .handle::<notif::PublishDiagnostics>()
        .log_unhandled();
    }

    pub(super) fn progress_tokens(&mut self, lsp_id: usize) -> &mut HashMap<RequestId, String> {
        self.progress_tokens.entry(lsp_id).or_default()
    }

    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;

        id
    }

    fn send_status(&self, message: impl Into<String>) {
        _ = self.tx_events.send(Event::Action(Action::SetStatusMessage {
            message: message.into(),
        }));
    }

    #[inline]
    fn report_error(&self, message: impl Into<String>) {
        let message = message.into();
        error!("{message}");
        self.send_status(message);
    }

    fn start_client(
        &mut self,
        lang: String,
        cmd: String,
        args: Vec<String>,
        root: String,
        open_bufs: Vec<PendingParams>,
    ) {
        let lsp_id = self.next_id();
        match LspClient::new(lsp_id, &cmd, args, self.tx_req.clone()) {
            Ok(client) => self.clients.insert(lsp_id, client),
            Err(e) => {
                return self.report_error(format!("failed to start LSP server: {e}"));
            }
        };

        Initialize::send(lsp_id, root, (lang, open_bufs), self);
        self.send_status("LSP server started");
    }

    fn stop_client(&mut self, lsp_id: usize) {
        use lsp_types::{notification::Exit, request::Shutdown};

        Shutdown::send(lsp_id, (), (), self);
        Exit::send(lsp_id, (), self);

        match self.clients.remove(&lsp_id) {
            Some(client) => client.join(),
            None => self.report_error("no attached LSP server"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Pos {
    pub(crate) file: String,
    pub(crate) line: u32,
    pub(crate) character: u32,
}

impl Pos {
    fn new(file: impl Into<String>, line: u32, character: u32) -> Self {
        Self {
            file: file.into(),
            line,
            character,
        }
    }
}

#[derive(Debug)]
pub(crate) struct PendingRequest {
    lsp_id: usize,
    pending: PendingParams,
}

#[derive(Debug)]
pub(crate) enum PendingParams {
    DocumentChange {
        path: String,
        content: String,
        version: usize,
    },
    DocumentClose {
        path: String,
    },
    DocumentOpen {
        lang: String,
        path: String,
        content: String,
    },
    FindReferences(Pos),
    GotoDeclaration(Pos),
    GotoDefinition(Pos),
    GotoTypeDefinition(Pos),
    Hover(Pos),
}

#[derive(Debug)]
pub(crate) enum Pending {
    FindReferences,
    GotoDeclaration,
    GotoDefinition,
    GotoTypeDefinition,
    Hover,
    Initialize(String, Vec<PendingParams>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diagnostic {
    path: String,
    content: String,
    coords: Coords,
}

impl Diagnostic {
    fn new(uri: Uri, d: lsp_types::Diagnostic, encoding: PositionEncoding) -> Self {
        let loc = lsp_types::Location {
            uri: uri.clone(),
            range: d.range,
        };
        let (path, coords) = Coords::new(loc, encoding);
        let fname = path.split("/").last().unwrap();
        let source = d.source.map(|s| format!("({s}) ")).unwrap_or_default();
        let content = format!("{source}{fname}:{} {}", coords.line(), d.message);

        Diagnostic {
            path,
            content,
            coords,
        }
    }

    pub fn as_actions(&self) -> Actions {
        Actions::Multi(vec![
            Action::OpenFile {
                path: self.path.clone(),
            },
            Action::DotSetFromCoords {
                coords: self.coords,
            },
            Action::SetViewPort(ViewPort::Center),
        ])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostics(Vec<Diagnostic>);

impl MbSelect for Diagnostics {
    fn clone_selector(&self) -> MbSelector {
        self.clone().into_selector()
    }

    fn prompt_and_options(&self, _: &Buffers) -> (String, Vec<String>) {
        (
            "Diagnostics> ".to_owned(),
            self.0.iter().map(|d| d.content.clone()).collect(),
        )
    }

    fn selected_actions(&self, sel: MiniBufferSelection) -> Option<Actions> {
        match sel {
            MiniBufferSelection::Line { cy, .. } => self.0.get(cy).map(|d| d.as_actions()),
            _ => None,
        }
    }
}
