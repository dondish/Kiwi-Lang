use std::{collections::HashMap, ops::{Add, Sub}};

use crate::parser::{AST, ASTNode, BinaryExpressionType};

pub struct Runner {
    ast: AST,
    functions: HashMap<String, ASTNode>,
    variables: Vec<HashMap<String, Value>>
}

pub struct RunError {
    pub kind: RunErrorKind
}
pub enum RunErrorKind {
    UndefinedVariable,
    InvalidOperation
}

pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Identifier(String)
}

macro_rules! enum_check_val {
    ($fn_name:ident, $type:pat) => {
        pub fn $fn_name(&self) -> bool {
            if let $type = self {
                true
            } else {
                false
            }
        }
    };
}

impl Value {

    enum_check_val!(is_int, Value::Int(_));
    enum_check_val!(is_float, Value::Float(_));
    enum_check_val!(is_string, Value::String(_));
    enum_check_val!(is_identifier, Value::Identifier(_));

    pub fn to_float(&self) -> Result<f64, RunError> {
        if let Self::Int(i) = self {
            Ok(i.to_owned() as f64)
        } else if let Self::Float(f) = self {
            Ok(f.to_owned())
        } else {
            Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })
        }
    }

    pub fn to_int(&self) -> Result<i64, RunError> {
        if let Self::Int(i) = self {
            Ok(i.to_owned())
        } else if let Self::Float(f) = self {
            Ok(f.to_owned() as i64)
        } else {
            Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })
        }
    }
}

impl Add for &Value {
    type Output = Result<Value, RunError>;

    fn add(self, other: Self) -> Self::Output {
        if self.is_identifier() || other.is_identifier() {
            Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })
        } else if self.is_string() || other.is_string() {
            Ok(Value::String(self.to_string() + &other.to_string()))
        } else if self.is_float() || other.is_float() {
            Ok(Value::Float(self.to_float()? + other.to_float()?))
        } else {
            Ok(Value::Int(self.to_int()? + other.to_int()?))
        }

    }
}

impl Sub for &Value {
    type Output = Result<Value, RunError>;
    
    fn sub(self, other: Self) -> Self::Output {
        if self.is_identifier() || other.is_identifier() || self.is_string() || other.is_string() {
            Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })
        } else if self.is_float() || other.is_float() {
            Ok(Value::Float(self.to_float()? - other.to_float()?))
        } else {
            Ok(Value::Int(self.to_int()? - other.to_int()?))
        }
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s.to_owned(),
            Value::Identifier(i) => i.to_owned()
        }
    }
}

impl Runner {
    pub fn run(self: &mut Runner) -> Result<(), RunError {
        for node in self.ast.root_nodes {
            self.run_node(node.as_ref())?;
        }
        Ok(())
    }

    fn run_node(self: &mut Runner, node: &ASTNode) -> Result<Value, RunError> {
        match node {
            ASTNode::BinaryExpression { expression_type, left_argument, right_argument} => {
                let left_value = self.run_node(left_argument)?;
                let right_value = self.run_node(right_argument)?;

                self.evaluate_binary_expression(expression_type, &left_value, &right_value)
            },
            ASTNode::CodeBlock {lines} => {
                for line in lines {
                    if let ASTNode::ReturnStatement = 
                    self.run_node(line.as_ref())?;
                }
            }
        }
    }

    fn evaluate_binary_expression(self: &mut Runner, expression_type: &BinaryExpressionType, left_argument: &Value, right_argument: &Value) -> Result<Value, RunError> {
        match expression_type {
            BinaryExpressionType::Add => {
                let left_argument_ensured = self.ensure_not_identifier(left_argument)?;
                let right_argument_ensured = self.ensure_not_identifier(left_argument)?;

                left_argument_ensured + right_argument_ensured
            },
            BinaryExpressionType::Subtract => {
                let left_argument_ensured = self.ensure_not_identifier(left_argument)?;
                let right_argument_ensured = self.ensure_not_identifier(left_argument)?;

                left_argument_ensured - right_argument_ensured
            },
            BinaryExpressionType::Assign => {
                if let Value::Identifier(ident) = left_argument {
                    let right_argument_ensured = self.ensure_not_identifier(left_argument)?;

                    self.set_variable(ident, right_argument_ensured);
                    Ok(right_argument)
                } else {
                    Err(RunError {
                        kind: RunErrorKind::InvalidOperation
                    })
                }
            }
        }
    }

    fn lookup_variable(self: &Runner, identifier: &String) -> Option<&Value> {
        for scope in self.variables.iter().rev() {
            if let Some(val) = scope.get(identifier) {
                return Some(val);
            }
        }

        None
    }

    fn ensure_not_identifier<'a>(self: &'a Runner, value: &'a Value) -> Result<&'a Value, RunError> {
        if let Value::Identifier(ident) = value {
            self.lookup_variable(ident).ok_or(RunError {
                kind: RunErrorKind::UndefinedVariable
            })
        } else {
            Ok(value)
        }
    }

    fn set_variable(self: &mut Runner, identifier: String, value: Value) {
        let scope = self.variables.last_mut().unwrap();

        scope.insert(identifier, value);
    }
}