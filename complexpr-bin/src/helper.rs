use std::borrow::Cow;

use complexpr::env::EnvRef;
use rustyline::{completion::Completer, validate::Validator};
use rustyline::highlight::Highlighter;
use rustyline::hint::HistoryHinter;
use rustyline::validate::{MatchingBracketValidator, ValidationResult, ValidationContext};
use rustyline_derive::{Helper, Hinter};

#[derive(Helper, Hinter)]
pub struct CxprHelper {
    #[rustyline(Validator)]
    pub validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    pub hinter: HistoryHinter,
    pub colored_prompt: String,
    pub env: EnvRef
}

fn find_paired_bracket(line: &str, pos: usize) -> Result<usize, bool> {
    if pos >= line.len() { 
        return Err(false) 
    }
    let c = line.as_bytes()[pos];
    let (target, fwd) = match c {
        b'(' => (b')', true),
        b')' => (b'(', false),
        b'[' => (b']', true),
        b']' => (b'[', false),
        b'{' => (b'}', true),
        b'}' => (b'{', false),
        _ => return Err(false),
    };
    let mut depth = 0;
    let mut idx = 0;
    if fwd {
        let bytes = &line.as_bytes()[pos+1..];
        for &b in bytes {
            if b == c {
                depth += 1;
            } else if b == target {
                if depth == 0 {
                    return Ok(pos + idx + 1)
                } else {
                    depth -= 1;
                }
            }
            idx += 1;
        }
    } else {
        let bytes = &line.as_bytes()[..pos];
        for &b in bytes.iter().rev() {
            if b == c {
                depth += 1;
            } else if b == target {
                if depth == 0 {
                    return Ok(pos - idx - 1)
                } else {
                    depth -= 1;
                }
            }
            idx += 1;
        }
    }
    Err(true)
}

impl Highlighter for CxprHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Cow::Borrowed(&self.colored_prompt)
        } else {
            Cow::Borrowed(prompt)
        }
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        match find_paired_bracket(line, pos) {
            Err(false) => Cow::Borrowed(line),
            Err(true) => {
                let mut line = line.to_owned();
                line.replace_range(pos..=pos, &format!("\x1b[91m{}\x1b[0m", line.as_bytes()[pos] as char));
                Cow::Owned(line)
            },
            Ok(match_pos) => {
                let fst = pos.min(match_pos);
                let snd = pos.max(match_pos);
                let mut line = line.to_owned();
                line.replace_range(snd..=snd, &format!("\x1b[92m{}\x1b[0m", line.as_bytes()[snd] as char));
                line.replace_range(fst..=fst, &format!("\x1b[92m{}\x1b[0m", line.as_bytes()[fst] as char));
                Cow::Owned(line)
            },
        }
    }

    fn highlight_char(&self, line: &str, _: usize) -> bool {
        !line.is_empty()
    }
}

fn validate_brackets(input: &str) -> ValidationResult {
    let mut stack = vec![];
    let mut in_string = false;
    let mut in_char = false;
    let mut in_escape = false;
    for c in input.chars() {
        if in_string {
            if in_escape { 
                in_escape = false
            } else if c == '\\' {
                in_escape = true
            } else if c == '"' {
                in_string = false
            }
        } else if in_char {
            if in_escape { 
                in_escape = false
            } else if c == '\\' {
                in_escape = true
            } else if c == '\'' {
                in_char = false
            }
        } else {
            match c {
                '(' | '[' | '{' => stack.push(c),
                ')' | ']' | '}' => match (stack.pop(), c) {
                    (Some('('), ')') | (Some('['), ']') | (Some('{'), '}') => (),
                    (Some(c), _) => return ValidationResult::Invalid(
                        Some(format!(" << Mismatched brackets: {:?} is not properly closed", c))
                    ),
                    (None, c) => return ValidationResult::Invalid(
                        Some(format!(" << Mismatched brackets: {:?} is unpaired", c))
                    ),
                },
                '"' => in_string = true,
                '\'' => in_char = true,
                _ => {}
            }
        }
    }
    if in_string {
        ValidationResult::Incomplete
    } else if stack.is_empty() {
        ValidationResult::Valid(None)
    } else {
        ValidationResult::Incomplete
    }
}

impl Validator for CxprHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        Ok(validate_brackets(ctx.input()))
    }
}

impl Completer for CxprHelper {
    fn complete(&self, line: &str, pos: usize, _: &rustyline::Context<'_>) 
    -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let mut res = String::new();
        for ch in line[..pos].chars().rev() {
            match ch {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => res.push(ch),
                _ => break
            }
        }
        let res: String = res.chars().rev().collect();
        let mut keys = self.env.borrow().items().keys()
            .filter(|x| x.starts_with(&res))
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        keys.sort();
        Ok((pos - res.len(), keys))
    }

    fn update(&self, line: &mut rustyline::line_buffer::LineBuffer, start: usize, elected: &str) {
        let end = line.pos();
        line.replace(start..end, elected);
    }

    type Candidate = String;
}
