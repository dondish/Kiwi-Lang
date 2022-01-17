use std::{collections::HashMap, ops::{Add, Sub, Not}};

use crate::parser::{AST, ASTNode, BinaryExpressionType, LiteralKind, UnaryExpressionType};

#[warn(dead_code)]
pub struct Runner {
    functions: HashMap<String, ASTNode>,
    variables: Vec<HashMap<String, Value>>
}

#[derive(Debug)]
pub struct RunError {
    pub kind: RunErrorKind
}

#[derive(Debug)]
pub enum RunErrorKind {
    UndefinedVariable,
    InvalidOperation
}

#[derive(Debug, Clone)]
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

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl From<String> for Value {
    fn from(str: String) -> Self {
        Value::String(str)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Int(if b { 1 } else { 0 })
    }
}

impl Not for &Value {
    type Output = Result<Value, RunError>;

    fn not(self) -> Self::Output {
        match self {
            Value::Int(i) => Ok((i.to_owned() == 0).into()),
            Value::Float(f) => Ok((f.to_owned() == 0.).into()),
            Value::String(s) => Ok((s == "").into()),
            Value::Identifier(_) => Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })

        }
    }
}

impl Runner {
    pub fn new() -> Self {
        Self { functions: HashMap::new(), variables: vec![HashMap::new()] }
    }

    pub fn run(self: &mut Runner, ast: &AST) -> Result<(), RunError> {
        for node in &ast.root_nodes {
            self.run_node(node.as_ref())?;
        }
        Ok(())
    }

    fn run_node(self: &mut Runner, node: &ASTNode) -> Result<Value, RunError> {
        match node {
            ASTNode::BinaryExpression { expression_type, left_argument, right_argument} => {
                let left_node = &self.run_node(left_argument)?;
                let right_node = &self.run_node(right_argument)?;
                self.evaluate_binary_expression(expression_type, left_node, right_node)
            },
            ASTNode::CodeBlock {lines} => self.evaluate_code_block(lines),
            ASTNode::FunctionCall { function_arguments, .. } => {
                println!("{}", function_arguments
                    .iter()
                    .map(|node| self.run_node(node))
                    .map(|val| val.map(|v| self.ensure_not_identifier(&v).map(|v| v.to_owned())))
                    .map(|node| node.map(|n| n.to_string()))
                    .collect::<Result<Vec<_>,_>>()?
                    .join(" ")
                );
                Ok(Value::Int(0))
            },
            ASTNode::FunctionDefinition { .. } => {
                Err(RunError {
                    kind: RunErrorKind::InvalidOperation
                })
            },
            ASTNode::Identifier(str) => Ok(Value::Identifier(str.to_owned())),
            ASTNode::Literal(kind) => Ok(match kind {
                LiteralKind::IntLiteral(i) => Value::Int(i.to_owned()),
                LiteralKind::StringLiteral(str) => Value::String(str.to_owned()),
                LiteralKind::FloatLiteral(float) => Value::Float(float.to_owned())
            }),

            ASTNode::UnaryExpression { expression_type, argument } => {
                let argument_value = &self.run_node(argument)?;

                self.evaluate_unary_expression(expression_type, argument_value)
            },
            _ => Err(RunError{
                kind: RunErrorKind::InvalidOperation
            })

        }
    }

    fn evaluate_binary_expression(self: &mut Runner, expression_type: &BinaryExpressionType, left_argument: &Value, right_argument: &Value) -> Result<Value, RunError> {
        match expression_type {
            BinaryExpressionType::Add => {
                let left_argument_ensured = self.ensure_not_identifier(left_argument)?;
                let right_argument_ensured = self.ensure_not_identifier(right_argument)?;

                left_argument_ensured + right_argument_ensured
            },
            BinaryExpressionType::Subtract => {
                let left_argument_ensured = self.ensure_not_identifier(left_argument)?;
                let right_argument_ensured = self.ensure_not_identifier(right_argument)?;

                left_argument_ensured - right_argument_ensured
            },
            BinaryExpressionType::Assign => {
                if let Value::Identifier(ident) = left_argument {
                    let right_argument_ensured = self.ensure_not_identifier(right_argument)?.to_owned();

                    self.set_variable(ident.to_owned(), right_argument_ensured.clone());
                    Ok(right_argument_ensured)
                } else {
                    Err(RunError {
                        kind: RunErrorKind::InvalidOperation
                    })
                }
            }
        }
    }

    fn evaluate_unary_expression(self: &mut Runner, expression_type: &UnaryExpressionType, argument: &Value) -> Result<Value, RunError> {
        match expression_type {
            UnaryExpressionType::Not => {
                let left_argument_ensured = self.ensure_not_identifier(argument)?;

                !left_argument_ensured
            }
        }
    }

    fn evaluate_code_block(self: &mut Runner, lines: &Vec<Box<ASTNode>>) -> Result<Value, RunError> {
        for line in lines {
            if let ASTNode::ReturnStatement {expression} = line.as_ref() {
                return self.run_node(expression);
            }
            self.run_node(line.as_ref())?;
        }
        Ok(Value::Int(0))
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