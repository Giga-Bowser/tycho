use std::fmt;

pub(crate) struct Indented<'a, F: ?Sized> {
    f: &'a mut F,
    needs_indent: bool,
}

impl<F: fmt::Write + ?Sized> fmt::Write for Indented<'_, F> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for (idx, line) in s.split('\n').enumerate() {
            if idx > 0 {
                self.f.write_char('\n')?;
                self.needs_indent = true;
            }

            if self.needs_indent {
                if line.is_empty() {
                    continue;
                }

                self.f.write_str("    ")?;
                self.needs_indent = false;
            }

            write!(self.f, "{line}")?;
        }

        Ok(())
    }
}

pub(crate) fn indented<D: ?Sized>(f: &mut D) -> Indented<'_, D> {
    Indented {
        f,
        needs_indent: true,
    }
}
