use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::value::Value;

#[derive(Debug)]
pub struct Environment {
    parent: Option<EnvRef>,
    map: HashMap<Rc<str>, Value>,
}

pub type EnvRef = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> Self {
        Self { parent: None, map: HashMap::new() }
    }

    pub fn wrap(self) -> EnvRef {
        Rc::new(RefCell::new(self))
    }

    pub fn extend(parent: EnvRef) -> Self {
        Self { parent: Some(parent), map: HashMap::new() }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.map.get(name) {
            Some(v) => Some(v.clone()),
            None => match self.parent {
                Some(ref p) => p.borrow().get(name),
                None => None
            }
        }
    }

    pub fn declare(&mut self, name: Rc<str>, value: Value) {
        self.map.insert(name, value);
    }

    #[allow(clippy::result_unit_err)]
    pub fn set(&mut self, name: Rc<str>, value: Value) -> Result<(),()> {
        match self.map.contains_key(&name) {
            true => { self.map.insert(name, value); Ok(()) },
            false => match self.parent {
                Some(ref mut p) => p.borrow_mut().set(name, value),
                None => Err(())
            }
        }
    }

    pub fn items(&self) -> &HashMap<Rc<str>, Value> {
        &self.map
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
