#![cfg(feature = "repl")]

use rustyline::{self, error::ReadlineError, Config, CompletionType, EditMode, hint::HistoryHinter, validate::MatchingBracketValidator, Editor};
use complexpr::{interpreter::interpret, value::Value};


use crate::{helper::CxprHelper, create_env};

const C_RESET: &str = "\x1b[0m";
const C_BLUE: &str = "\x1b[94m";
const C_RED: &str = "\x1b[91m";
const PROMPT: &str = "\x1b[94m>> \x1b[0m";

pub fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();

    let env = create_env().wrap();

    let h = CxprHelper {
        hinter: HistoryHinter {},
        colored_prompt: PROMPT.to_owned(),
        validator: MatchingBracketValidator::new(),
        env: env.clone(),
    };

    let histfile = std::env::var("COMPLEXPR_HISTORY").ok();

    let mut rl = Editor::with_config(config)?;
    rl.set_helper(Some(h));
    if let Some(hf) = &histfile {
        rl.load_history(hf)?;
    }

    println!("Press {}Ctrl+D{} to exit.", C_BLUE, C_RESET);

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                let result = interpret(&line, None, Some(env.clone()), true);
                match result {
                    Ok(value) => {
                        if value != Value::Nil {
                            println!("{}", value.repr());
                        }
                        env.borrow_mut().declare("_".into(), value);
                    }
                    Err(e) => eprintln!("{}Error: {}{}", C_RED, C_RESET, e)
                }
            }
            Err(ReadlineError::Eof) => break,
            Err(_) => (),
        }
    }

    if let Some(hf) = &histfile {
        rl.save_history(hf)?
    }
    Ok(())
}
