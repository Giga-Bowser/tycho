use std::{
    fs,
    io::{self, Read},
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::utils::{Span, SrcLoc};

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: Box<Path>,
    pub src: Rc<str>,
    pub start_pos: SrcLoc,
}

impl SourceFile {
    pub fn read(path: &Path, start_pos: SrcLoc) -> io::Result<Self> {
        let src = if path == Path::new("-") {
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf)?;
            buf.into()
        } else {
            fs::read_to_string(path)?.into()
        };

        Ok(Self {
            path: Box::from(path),
            src,
            start_pos,
        })
    }
}

#[derive(Default)]
pub struct SourceMap {
    pub files: Vec<Rc<SourceFile>>,
}

impl SourceMap {
    pub const fn new() -> Self {
        Self { files: Vec::new() }
    }

    pub fn load_file(&mut self, path: impl AsRef<Path>) -> io::Result<Rc<SourceFile>> {
        let start_pos = self
            .files
            .last()
            .map_or(0, |it| it.start_pos + it.src.len() as SrcLoc);
        self.files
            .push(Rc::new(SourceFile::read(path.as_ref(), start_pos)?));
        Ok(self.files.last().unwrap().clone())
    }

    pub fn add_sources(&mut self, mut sources: Vec<PathBuf>) -> io::Result<()> {
        while let Some(cur) = sources.pop() {
            if cur.is_dir() {
                let files = fs::read_dir(cur)?
                    .map(|entry| entry.map(|it| it.path()))
                    .collect::<io::Result<Vec<PathBuf>>>()?;
                sources.extend(files.into_iter());
            } else {
                self.load_file(&cur)?;
            }
        }

        Ok(())
    }

    pub fn span_file(&self, span: Span) -> Option<&Rc<SourceFile>> {
        self.files.iter().find(|file| {
            file.start_pos <= span.start && (file.start_pos + file.src.len() as SrcLoc) >= span.end
        })
    }
}
