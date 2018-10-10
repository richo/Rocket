use std::fmt::Debug;
use std::marker::PhantomData;

use http::Header;
use request::Request;
use response::{self, Responder, Response};

/// A file with an associated name; responds with the Content-Type based on the
/// file extension.
#[derive(Debug)]
pub struct Attachment<R> {
    pub responder: R,
    pub filename: String,
}

impl<R> Attachment<R> {

}

/// Streams the named file to the client. Sets or overrides the Content-Type in
/// the response according to the file's extension if the extension is
/// recognized. See
/// [ContentType::from_extension](/rocket/http/struct.ContentType.html#method.from_extension)
/// for more information. If you would like to stream a file with a different
/// Content-Type than that implied by its extension, use a `File` directly.
impl<'r, R> Responder<'r> for Attachment<R>
where R: Debug + Responder<'r> {
    fn respond_to(self, req: &Request) -> response::Result<'r> {
        // TODO(richo) fail if the provided filename is invalid
        Response::build()
            .merge(self.responder.respond_to(req)?)
            .header(Header::new("Content-Disposition", format!("attachment; filename=\"{}\"", self.filename)))
            .ok()
    }
}
