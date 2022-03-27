use std::{collections::HashMap, ops::{Add, Sub, Not}};

use crate::parser::{AST, ASTNode, BinaryExpressionType, LiteralKind, UnaryExpressionType};

pub type ExternalFunction = fn(&Vec<Value>) -> Result<Value, RunError>;

#[warn(dead_code)]
pub struct Runner<'a> {
    functions: HashMap<String, Function<'a>>,
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

#[derive(Clone, Copy)]
pub enum Function<'a> {
    InternalFunction(&'a ASTNode),
    ExternalFunction(ExternalFunction)
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

impl <'runner> Runner<'runner> {
    pub fn new(external_functions: &HashMap<String, ExternalFunction>) -> Self {
        let functions: HashMap<String, Function> = external_functions.iter().map(|val| {
            let (name, definition) = val;
            (name.to_owned(), Function::ExternalFunction(definition.to_owned()))
        }).collect();

        Self { functions, variables: vec![HashMap::new()] }
    }

    pub fn run(self: &mut Runner<'runner>, ast: &'runner AST) -> Result<(), RunError> {
        for node in &ast.root_nodes {
            self.run_node(node.as_ref())?;
        }
        if let Some(function) = self.functions.get("main") { // Evaluate main function
            let function = *function;
            self.evaluate_function_body(function, &vec![])?;
        }
        Ok(())
    }

    fn run_node(self: &mut Runner<'runner>, node: &'runner ASTNode) -> Result<Value, RunError> {
        match node {
            ASTNode::BinaryExpression { expression_type, left_argument, right_argument} => {
                let left_node = &self.run_node(left_argument)?;
                let right_node = &self.run_node(right_argument)?;
                self.evaluate_binary_expression(expression_type, left_node, right_node)
            },
            ASTNode::CodeBlock {lines} => self.evaluate_code_block(lines),
            ASTNode::FunctionCall { function_arguments, function } => self.evaluate_function_call(function, function_arguments),
            ASTNode::FunctionDefinition { function_name, .. } => {
                self.functions.insert(self.get_function_name_from_identifier(&function_name)?, Function::InternalFunction(node));
                Ok(Value::Int(0))
            },
            ASTNode::Identifier(_) => self.evaluate_identifier(node),
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

    fn convert_node_to_value(self: &mut Runner<'runner>, node: &'runner ASTNode) -> Result<Value, RunError> {
        let value = self.run_node(node)?;
        Ok(self.ensure_not_identifier(&value)?.to_owned())
    }

    fn evaluate_binary_expression(self: &mut Runner<'runner>, expression_type: &BinaryExpressionType, left_argument: &Value, right_argument: &Value) -> Result<Value, RunError> {
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

    fn evaluate_unary_expression(self: &mut Runner<'runner>, expression_type: &UnaryExpressionType, argument: &Value) -> Result<Value, RunError> {
        match expression_type {
            UnaryExpressionType::Not => {
                let left_argument_ensured = self.ensure_not_identifier(argument)?;

                !left_argument_ensured
            }
        }
    }

    fn evaluate_identifier(self: &mut Runner<'runner>, identifier: &'runner ASTNode) -> Result<Value, RunError> {
        if let Ok(function) = self.get_function_from_name(identifier) {
            self.evaluate_function_body(function, &vec![])
        } else {
            Ok(Value::Identifier(self.get_function_name_from_identifier(identifier)?))
        }
    }

    fn evaluate_code_block(self: &mut Runner<'runner>, lines: &'runner Vec<Box<ASTNode>>) -> Result<Value, RunError> {
        self.increase_scope_level();
        for line in lines {
            if let ASTNode::ReturnStatement {expression} = line.as_ref() {
                let res = self.run_node(expression)?;
                self.decrease_scope_level();
                return Ok(res)
            }
            self.run_node(line.as_ref())?;
        }
        self.decrease_scope_level();
        Ok(Value::Int(0))
    }

    fn evaluate_function_call(self: &mut Runner<'runner>, function_name: &ASTNode, arguments: &'runner Vec<Box<ASTNode>>) -> Result<Value, RunError> {
        let values: Vec<_> = self.convert_function_arguments_to_values(arguments)?;
        let function = self.get_function_from_name(function_name)?;
        self.evaluate_function_body(function, &values)
    }

    fn get_function_from_name(self: &Runner<'runner>, function: &ASTNode) -> Result<Function<'runner>, RunError> {
        let function_name = self.get_function_name_from_identifier(function)?;
        self.functions.get(&function_name).map(|x| *x).ok_or(RunError {
            kind: RunErrorKind::UndefinedVariable
        })
    }

    fn get_function_name_from_identifier(self: &Runner<'runner>, identifier: &ASTNode) -> Result<String, RunError> {
        if let ASTNode::Identifier(name) = identifier {
            Ok(name.to_owned())
        } else {
            Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })
        }
    }

    fn convert_function_arguments_to_values(self: &mut Runner<'runner>, arguments: &'runner Vec<Box<ASTNode>>) -> Result<Vec<Value>, RunError> {
        arguments.iter().map(|arg| self.convert_node_to_value(arg)).collect()
    }

    fn evaluate_function_body(self: &mut Runner<'runner>, function: Function<'runner>, values: &Vec<Value>) -> Result<Value, RunError> {
        match function {
            Function::InternalFunction(function_node) => self.evaluate_internal_function(function_node, values),
            Function::ExternalFunction(external_func) => self.evaluate_external_function(external_func, values)
        }
    }

    fn evaluate_internal_function(self: &mut Runner<'runner>, function: &'runner ASTNode, values: &Vec<Value>) -> Result<Value, RunError> {
        self.set_function_call_variables(function, values)?;
        self.increase_scope_level();
        let res = self.evaluate_internal_function_body(function)?;
        self.decrease_scope_level();
        Ok(res)
    }

    fn evaluate_internal_function_body(self: &mut Runner<'runner>, function: &'runner ASTNode) -> Result<Value, RunError> {
        if let ASTNode::FunctionDefinition { function_code, .. } = function {
            if let ASTNode::CodeBlock { lines } = &**function_code {
                let res = self.evaluate_code_block(lines)?;
                self.decrease_scope_level();
                Ok(res)
            } else {
                Err(RunError {
                    kind: RunErrorKind::InvalidOperation
                })
            }
        } else {
            Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })
        }
    }

    fn set_function_call_variables(self: &mut Runner<'runner>, function: &ASTNode, values: &Vec<Value>) -> Result<(), RunError> {
        if let ASTNode::FunctionDefinition { function_arguments, .. } = function {
            let function_argument_idenifiers = Runner::get_function_argument_identifiers_as_strs(function_arguments)?;

            for arg_value in function_argument_idenifiers.iter().zip(values.iter()) {
                let (arg, value) = arg_value;
                self.set_variable(arg.to_owned(), value.to_owned());
            }


            Ok(())
        } else {
            Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })
        }
    }

    fn evaluate_external_function(self: &mut Runner<'runner>, function: ExternalFunction, values: &Vec<Value>) -> Result<Value, RunError> {
        function(values)
    }

    fn get_function_argument_identifiers_as_strs(argument_names: &Vec<Box<ASTNode>>) -> Result<Vec<String>, RunError> {
        argument_names.iter()
                .map(|arg| Runner::convert_ast_identifier_node_to_string(arg))
                .map(|res| res.map(|x| x.to_string()))
                .collect::<Result<Vec<String>, _>>()
    }

    fn convert_ast_identifier_node_to_string<'a>(identifier: &'a ASTNode) -> Result<&'a String, RunError> {
        if let ASTNode::Identifier(ident) = identifier {
            Ok(ident)
        } else {
            Err(RunError {
                kind: RunErrorKind::InvalidOperation
            })
        }
    }

    fn lookup_variable(self: &Runner<'runner>, identifier: &String) -> Option<&Value> {
        for scope in self.variables.iter().rev() {
            if let Some(val) = scope.get(identifier) {
                return Some(val);
            }
        }

        None
    }

    fn ensure_not_identifier<'a>(self: &'a Runner<'runner>, value: &'a Value) -> Result<&'a Value, RunError> {
        if let Value::Identifier(ident) = value {
            self.lookup_variable(ident).ok_or(RunError {
                kind: RunErrorKind::UndefinedVariable
            })
        } else {
            Ok(value)
        }
    }

    fn set_variable(self: &mut Runner<'runner>, identifier: String, value: Value) {
        let scope = self.variables.last_mut().unwrap();

        scope.insert(identifier, value);
    }

    fn increase_scope_level(self: &mut Runner<'runner>) {
        self.variables.push(HashMap::new())
    }

    fn decrease_scope_level(self: &mut Runner<'runner>) {
        self.variables.pop();
    }
}