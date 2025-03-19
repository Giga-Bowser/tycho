use std::{error::Error, fmt, io::BufWriter, path::Path, rc::Rc};

use ariadne::{Color, Label, Report, ReportBuilder, ReportKind, Source};
use rustc_hash::FxHashMap;

use crate::{
    parser::pool::ExprPool,
    sourcemap::{SourceFile, SourceMap},
    typecheck::ctx::TypeContext,
    utils::Span,
};

pub(crate) fn report_err(
    err: &impl Snippetize,
    ctx: &DiagCtx<'_>,
    map: &SourceMap,
) -> Result<String, Box<dyn Error>> {
    report_diag(err.snippetize(ctx), map)
}

pub(crate) fn report_diag(diag: Diag, map: &SourceMap) -> Result<String, Box<dyn Error>> {
    let Diag {
        title,
        level,
        annotations,
        notes,
    } = diag;

    let span = annotations.first().map(|it| it.span).unwrap_or_default();

    let mut builder =
        Report::build(level.report_kind(), WrappedSpan::new(map, span)).with_message(&title);

    for annotations in &annotations {
        builder = annotations.apply(builder, map);
    }

    builder.with_notes(notes);

    let mut buf = BufWriter::new(Vec::new());
    builder.finish().write(SourceCache::new(map), &mut buf)?;

    let bytes = buf.into_inner()?;
    Ok(String::from_utf8(bytes)?)
}

pub(crate) struct DiagCtx<'a> {
    pub tcx: &'a TypeContext,
    pub expr_pool: &'a ExprPool,
    pub file: &'a SourceFile,
}

pub(crate) trait Snippetize {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag;
}

#[derive(Debug)]
pub(crate) struct Diag {
    pub level: Level,
    pub title: String,
    pub annotations: Vec<Annotation>,
    pub notes: Vec<String>,
}

impl Diag {
    pub(crate) fn new(level: Level, title: impl Into<String>) -> Self {
        Diag {
            level,
            title: title.into(),
            annotations: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub(crate) fn add_annotation(&mut self, annotation: Annotation) {
        self.annotations.push(annotation);
    }

    #[must_use]
    pub(crate) fn with_annotation(mut self, annotation: Annotation) -> Self {
        self.add_annotation(annotation);
        self
    }

    pub(crate) fn add_note(&mut self, note: impl Into<String>) {
        self.notes.push(note.into());
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Annotation {
    pub level: Level,
    pub span: Span,
    pub label: Option<String>,
}

impl Annotation {
    pub(crate) fn new(level: Level, span: Span) -> Self {
        Annotation {
            level,
            span,
            label: None,
        }
    }

    pub(crate) fn label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }

    fn apply<'a>(
        &self,
        builder: ReportBuilder<'a, WrappedSpan<'a>>,
        map: &'a SourceMap,
    ) -> ReportBuilder<'a, WrappedSpan<'a>> {
        let mut label = Label::new(WrappedSpan::new(map, self.span)).with_color(self.level.color());

        if let Some(s) = &self.label {
            label = label.with_message(s);
        }

        builder.with_label(label)
    }
}

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Level {
    Error,
    Warning,
    Info,
    Note,
    Help,
}

impl Level {
    pub(crate) fn color(self) -> Color {
        match self {
            Level::Error => Color::Red,
            Level::Warning => Color::Yellow,
            Level::Info => Color::BrightBlue,
            Level::Note => Color::BrightGreen,
            Level::Help => Color::BrightCyan,
        }
    }

    pub(crate) fn report_kind(self) -> ReportKind<'static> {
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
    pub(crate) fn new(map: &'a SourceMap, span: Span) -> Self {
        let file = map.span_file(span).unwrap();
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

pub(crate) struct SourceCache {
    map: FxHashMap<Box<Path>, Source<Rc<str>>>,
}

impl SourceCache {
    pub(crate) fn new(map: &SourceMap) -> Self {
        let map = map
            .files
            .iter()
            .map(|file| (file.path.clone(), Source::from(file.src.clone())))
            .collect();
        SourceCache { map }
    }
}

impl ariadne::Cache<Path> for SourceCache {
    type Storage = Rc<str>;

    fn fetch(&mut self, path: &Path) -> Result<&Source<Self::Storage>, impl fmt::Debug> {
        self.map
            .get(path)
            .ok_or_else(|| format!("{} not found in source map", path.display()))
    }

    fn display<'a>(&self, path: &'a Path) -> Option<impl fmt::Display + 'a> {
        Some(Box::new(path.display()))
    }
}

#[macro_export]
macro_rules! error_kind {
    ($(#[$m:meta])* $v:vis enum $name:ident { $($var:ident),* $(,)? }) => {
        $(#[$m])*
        $v enum $name {
            $($var($var),)*
        }

        impl Snippetize for $name {
            fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
                match self {
                    $(Self::$var(inner) => inner.snippetize(ctx),)*
                }
            }
        }

        $(impl From<$var> for $name {
            fn from(value: $var) -> Self {
                Self::$var(value)
            }
        })*
    };
}
