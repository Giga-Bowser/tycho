use std::{error::Error, io::BufWriter, ops::Range};

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::{
    parser::pool::ExprPool, sourcemap::SourceFile, typecheck::ctx::TypeContext, utils::Span,
};

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
    pub range: Range<usize>,
    pub label: Option<String>,
}

impl Annotation {
    pub fn new(level: Level, range: Range<usize>) -> Self {
        Annotation {
            level,
            range,
            label: None,
        }
    }

    pub fn new_span(level: Level, span: Span) -> Self {
        Annotation {
            level,
            range: span.to_range(),
            label: None,
        }
    }

    pub fn label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }
}

pub struct DiagCtx<'a> {
    pub tcx: &'a TypeContext,
    pub expr_pool: &'a ExprPool,
    pub file: &'a SourceFile,
}

pub(crate) trait Snippetize {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag;
}

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
    let path = file.path.to_string_lossy();

    let range = annotations
        .first()
        .map(|it| it.range.clone())
        .unwrap_or_default();

    let report = Report::build(level.report_kind(), (&path, range))
        .with_message(&title)
        .with_labels(annotations.iter().map(|it| {
            let mut label = Label::new((&path, it.range.clone())).with_color(it.level.color());
            if let Some(s) = &it.label {
                label = label.with_message(s);
            }
            label
        }));

    let mut buf = BufWriter::new(Vec::new());
    report
        .finish()
        .write((&path, Source::from(&file.src)), &mut buf)?;

    let bytes = buf.into_inner()?;
    Ok(String::from_utf8(bytes)?)
}
