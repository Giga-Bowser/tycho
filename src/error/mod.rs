use std::ops::Range;

use ariadne::{Color, ReportKind};

use crate::{parser::pool::ExprPool, typecheck::ctx::TypeContext, utils::Span};

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

pub struct DiagCtx<'a, 's> {
    pub tcx: &'a TypeContext<'s>,
    pub expr_pool: &'a ExprPool,
    pub source: &'s str,
}

pub(crate) trait Snippetize<'s> {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag;
}
