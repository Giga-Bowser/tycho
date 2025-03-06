use std::ops::Range;

use ariadne::{Color, ReportKind};

use crate::{parser::pool::ExprPool, typecheck::ctx::TypeContext};

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
    pub title: String,
    pub level: Level,
    pub annotations: Vec<Annotation>,
}

#[derive(Debug)]
pub struct Annotation {
    pub level: Level,
    pub range: Range<usize>,
    pub label: Option<String>,
}

pub struct DiagCtx<'a, 's> {
    pub tcx: &'a TypeContext<'s>,
    pub expr_pool: &'a ExprPool<'s>,
    pub source: &'s str,
}

pub(crate) trait Snippetize<'s> {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag;
}
