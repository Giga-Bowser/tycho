use std::{error::Error, fmt, io::BufWriter, path::Path, rc::Rc};

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::{
    parser::pool::ExprPool, sourcemap::SourceFile, typecheck::ctx::TypeContext, utils::Span,
};

pub(crate) fn report_err(
    err: &impl Snippetize,
    ctx: &DiagCtx<'_>,
) -> Result<String, Box<dyn Error>> {
    report_diag(err.snippetize(ctx), ctx.file)
}

pub(crate) fn report_diag(diag: Diag, file: &SourceFile) -> Result<String, Box<dyn Error>> {
    let Diag {
        title,
        level,
        annotations,
    } = diag;

    let span = annotations.first().map(|it| it.span).unwrap_or_default();

    let report = Report::build(level.report_kind(), WrappedSpan::new(file, span))
        .with_message(&title)
        .with_labels(annotations.iter().map(|it| {
            let mut label =
                Label::new(WrappedSpan::new(file, it.span)).with_color(it.level.color());
            if let Some(s) = &it.label {
                label = label.with_message(s);
            }
            label
        }));

    let mut buf = BufWriter::new(Vec::new());
    report.finish().write(SourceCache::new(file), &mut buf)?;

    let bytes = buf.into_inner()?;
    Ok(String::from_utf8(bytes)?)
}

pub struct DiagCtx<'a> {
    pub tcx: &'a TypeContext,
    pub expr_pool: &'a ExprPool,
    pub file: &'a SourceFile,
}

pub(crate) trait Snippetize {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag;
}

#[derive(Debug)]
pub struct Diag {
    pub level: Level,
    pub title: String,
    pub annotations: Vec<Annotation>,
}

impl Diag {
    pub fn new(level: Level, title: impl Into<String>) -> Self {
        Diag {
            level,
            title: title.into(),
            annotations: Vec::new(),
        }
    }

    #[must_use]
    pub fn add_annotation(mut self, annotation: Annotation) -> Self {
        self.annotations.push(annotation);

        self
    }

    #[must_use]
    pub fn add_annotations(mut self, annotations: impl IntoIterator<Item = Annotation>) -> Self {
        self.annotations.extend(annotations);

        self
    }
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub level: Level,
    pub span: Span,
    pub label: Option<String>,
}

impl Annotation {
    pub fn new(level: Level, span: Span) -> Self {
        Annotation {
            level,
            span,
            label: None,
        }
    }

    pub fn label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Level {
    Error,
    Warning,
    Info,
    Note,
    Help,
}

impl Level {
    pub fn color(self) -> Color {
        match self {
            Level::Error => Color::Red,
            Level::Warning => Color::Yellow,
            Level::Info => Color::BrightBlue,
            Level::Note => Color::BrightGreen,
            Level::Help => Color::BrightCyan,
        }
    }

    pub fn report_kind(self) -> ReportKind<'static> {
        let s = match self {
            Level::Error => "error",
            Level::Warning => "warning",
            Level::Info => "info",
            Level::Note => "note",
            Level::Help => "help",
        };

        ReportKind::Custom(s, self.color())
    }
}

pub(crate) struct WrappedSpan<'a> {
    file: &'a SourceFile,
    span: Span,
}

impl<'a> WrappedSpan<'a> {
    pub(crate) const fn new(file: &'a SourceFile, span: Span) -> Self {
        WrappedSpan { file, span }
    }
}

impl ariadne::Span for WrappedSpan<'_> {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        &self.file.path
    }

    fn start(&self) -> usize {
        self.span.to_range(self.file).start
    }

    fn end(&self) -> usize {
        self.span.to_range(self.file).end
    }
}

pub(crate) struct SourceCache<'a> {
    file: &'a SourceFile,
    source: Source<Rc<str>>,
}

impl<'a> SourceCache<'a> {
    pub(crate) fn new(file: &'a SourceFile) -> Self {
        SourceCache {
            file,
            source: Source::from(file.src.clone()),
        }
    }
}

impl ariadne::Cache<Path> for SourceCache<'_> {
    type Storage = Rc<str>;

    fn fetch(&mut self, id: &Path) -> Result<&Source<Self::Storage>, Box<dyn fmt::Debug + '_>> {
        assert_eq!(id, self.file.path.as_ref()); // idk if this even means anything?
        Ok(&self.source)
    }

    fn display<'a>(&self, id: &'a Path) -> Option<Box<dyn fmt::Display + 'a>> {
        Some(Box::new(id.display()))
    }
}
