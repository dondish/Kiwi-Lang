use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{FunctionType, StructType},
    values::{BasicValue, FunctionValue, PointerValue, StructValue},
    AddressSpace,
};
use nom::HexDisplay;

use crate::parser::{ASTNode, BinaryExpressionType, LiteralKind, UnaryExpressionType, AST};

use super::kobject::KObjectType;

pub struct ModuleCompiler<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    variables: Vec<HashMap<String, PointerValue<'ctx>>>,
    entry_point_fn_value: Option<FunctionValue<'ctx>>,
    current_fn_value: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> ModuleCompiler<'a, 'ctx> {
    fn get_kobject_type(&self) -> StructType {
        let i64_type = self.context.i64_type(); // The value
        let i32_type = self.context.i32_type(); // The type
        self.context.struct_type(&[i64_type, i32_type], false)
    }

    fn get_kstring_type(&self) -> StructType {
        let ptr_type = self.context.i8_type().ptr_type(AddressSpace::Generic); // value
        let i64_type = self.context.i64_type(); // size
        self.context.struct_type(&[ptr_type, i64_type], false)
    }

    fn create_const_kstring(&self, string: &str) -> StructValue<'ctx> {
        let mut hasher = DefaultHasher::new();
        string.hash(&mut hasher);
        let hash = hasher.finish();
        let hash_hex = hash.to_le_bytes().to_hex(8);
        let const_string = self.context.const_string(string, false);
        let global = self.module.add_global(
            const_string.get_type(),
            Some(AddressSpace::Const),
            "g_str_" + &hash_hex,
        );
        global.set_initializer(&const_string);
        global.set_linkage(Linkage::Internal);
        let i64_type = self.context.i64_type();
        let size_value = i64_type.const_int(string.len(), false);
        self.get_kstring_type()
            .const_named_struct(&[global.as_pointer_value(), size_value])
    }

    fn create_const_int_kobject(&self, integer: i64) -> StructValue<'ctx> {
        let i64_type = self.context.i64_type();
        let i64_value = i64_type.const_int(integer, false);
        let i32_type = self.context.i32_type();
        let i32_value = i32_type.const_int(KObjectType::Int.into(), false);
        self.get_kobject_type()
            .const_named_struct(&[i64_value, i32_value])
    }

    fn create_const_float_kobject(&self, float: f64) -> StructValue<'ctx> {
        let f64_type = self.context.f64_type();
        let f64_value = f64_type.const_float(float);
        let i32_type = self.context.i32_type();
        let i32_value = i32_type.const_int(KObjectType::Float.into(), false);
        self.get_kobject_type()
            .const_named_struct(&[f64_value, i32_value])
    }

    fn create_const_string_kobject(&self, string: &str) -> StructValue<'ctx> {
        let mut hasher = DefaultHasher::new();
        string.hash(&mut hasher);
        let hash = hasher.finish();
        let hash_hex = hash.to_le_bytes().to_hex(8);
        let kstring = self.create_const_kstring(string);
        let allocated_kstring = self.get_alloca(self.get_kstring_type());
        self.builder.build_store(allocated_string, kstring);
        let i32_type = self.context.i32_type();
        let i32_value = i32_type.const_int(KObjectType::String.into(), false);
        self.get_kobject_type()
            .const_named_struct(&[allocated_kstring, i32_value])
    }

    fn get_default_zero_constant(&self) -> StructValue<'ctx> {
        return self.create_const_int_kobject(0);
    }

    fn get_variable(&self, name: &str) -> Option<PointerValue> {
        for item in self.variables.iter().rev() {
            if let Some(variable) = item.get(name) {
                return Some(variable.clone());
            }
        }
        None
    }

    fn get_function_type(&self, number_of_arguments: usize) {
        let kobject_ptr_type = self.get_kobject_type().ptr_type(AddressSpace::Generic);
        let args_types = std::iter::repeat(kobject_ptr_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        kobject_ptr_type.fn_type(args_types.as_slice(), is_var_args)
    }

    fn get_alloca<T: BasicType<'ctx>>(&mut self, value_type: T) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self
            .current_fn_value
            .unwrap()
            .get_first_basic_block()
            .unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(value_type, name)
    }

    fn set_variable(&mut self, name: &str, value: PointerValue) {
        value.set_name(name);
        self.variables
            .last()
            .unwrap()
            .insert(name.to_owned(), value);
    }

    fn get_binary_expression_name(&self, experssion_type: BinaryExpressionType) -> &'static str {
        match experssion_type {
            BinaryExpressionType::Add => "__KObject__add",
            BinaryExpressionType::Subtract => "__KObject__subtract",
            _ => "",
        }
    }

    fn get_binary_expression_fn_type(&mut self) -> FunctionType {
        self.get_function_type(2)
    }

    fn get_binary_expression_fn_value(
        &mut self,
        experssion_type: BinaryExpressionType,
    ) -> FunctionValue {
        let fn_name = self.get_binary_expression_name(experssion_type);
        if let Some(func) = self.module.get_function(fn_name) {
            func
        } else {
            self.module.add_function(
                fn_name,
                self.get_binary_expression_fn_type(),
                Some(Linkage::AvailableExternally),
            )
        }
    }

    fn get_unary_expression_name(&self, expression_type: UnaryExpressionType) -> &'static str {
        match expression_type {
            UnaryExpressionType::Not => "__KObject_lnot",
        }
    }

    fn get_unary_expression_fn_type(&mut self) -> FunctionType {
        self.get_function_type(1)
    }

    fn get_unary_expression_fn_value(
        &mut self,
        expression_type: UnaryExpressionType,
    ) -> FunctionValue {
        let fn_name = self.get_unary_expression_name(expression_type);

        if let Some(func) = self.module.get_function(fn_name) {
            func
        } else {
            self.module.add_function(
                fn_name,
                self.get_unary_expression_fn_type(),
                Some(Linkage::AvailableExternally),
            )
        }
    }

    fn compile_expr(&mut self, node: &ASTNode) -> Result<PointerValue<'ctx>, &str> {
        match *node {
            ASTNode::Literal(kind) => {
                let kobject = match kind {
                    LiteralKind::IntLiteral(i) => self.create_const_int_kobject(i),
                    LiteralKind::FloatLiteral(f) => self.create_const_float_kobject(f),
                    LiteralKind::StringLiteral(s) => self.create_const_string_kobject(&s),
                };
                let allocated_kobject = self.get_alloca(self.get_kobject_type());
                self.builder.build_store(allocated_kobject, kobject);
                Ok(allocated_kobject)
            }
            ASTNode::Identifier(ident) => self.get_variable(index).ok_or("No matching variable"),
            ASTNode::BinaryExpression {
                expression_type,
                left_argument,
                right_argument,
            } => {
                if let BinaryExpressionType::Assign = expression_type {
                    let ASTNode::Identifier(ident) = left_argument;
                    let value = self.compile_expr(&right_argument);
                    self.set_variable(&ident, value?);
                    return value;
                }
                let left = self.compile_expr(&left_argument)?;
                let right = self.compile_expr(&right_argument)?;
                let fn_value = self.get_binary_expression_fn_value(experssion_type);
                Ok(self
                    .builder
                    .build_call(fn_value, &[left, right], "bin_expr_call")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value())
            }
            ASTNode::UnaryExpression {
                expression_type,
                argument,
            } => {
                let arg = self.compile_expr(&argument)?;
                let fn_value = self.get_unary_expression_fn_value(expression_type);
                Ok(self
                    .builder
                    .build_call(fn_value, &[arg], "un_expr_call")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value())
            }
            ASTNode::FunctionCall {
                function,
                function_arguments,
            } => {
                let compiled_args: Result<Vec<PointerValue>, &str> = function_arguments
                    .iter()
                    .map({ |arg| self.compile_expr(&arg) })
                    .collect()?;
                let ASTNode::Identifier(ident) = function;
                let function = self.module.get_function(&ident).ok_or("unknown function")?;
                Ok(self
                    .builder
                    .build_call(fn_value, &compiled_args, "func_call")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value())
            }
            ASTNode::ReturnStatement { expression } => {
                let compiled_expression = self.compile_expr(&expression)?;
                Ok(self.builder.build_return(Some(&compiled_expression)))
            }
            ASTNode::FunctionDefinition {
                function_name,
                function_arguments,
                function_code,
            } => Err("Cannot make top-level functions"),
            ASTNode::CodeBlock { lines } => {
                for line in lines {
                    self.compile_expr(&line)?
                }
            }
        }
    }

    fn declare_fn(&self, name: &str, arguments: Vec<&ASTNode>) -> FunctionValue<'ctx> {
        let function =
            self.module
                .add_function(name, self.get_function_type(arguments.len()), None);

        for (i, arg) in function.get_param_iter().enumerate() {
            let ASTNode::Identifier(arg_name) = arguments[i];
            arg.into_pointer_value().set_name(&arg_name)
        }

        function
    }

    fn compile_fn(
        &mut self,
        name: &str,
        arguments: Vec<&ASTNode>,
        code: &ASTNode,
    ) -> Result<(), &'static str> {
        let function = self.declare_fn(name, arguments);
        let ASTNode::CodeBlock { lines } = code;

        self.current_fn_value = Some(function);

        let entry_block = self.context.append_basic_block(function, "entry");

        self.variables.push(HashMap::new());

        for (i, line) in lines.iter().enumerate() {
            let line_block = self.context.append_basic_block(function, "line-" + i);
            self.builder.position_at_end(line_block);
            self.compile_expr(line);
        }

        self.variables.pop();

        let last_block = self.context.append_basic_block(function, "last");
        self.builder.position_at_end(last_block);
        self.builder
            .build_return(Some(&self.get_default_zero_constant()))
    }

    fn compile_root_nodes(&mut self, root_nodes: &Vec<ASTNode>) {
        for node in root_nodes {}
    }

    fn create_main_function(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
    ) -> FunctionValue<'ctx> {
        let main_fn_type = context.i32_type().fn_type(&[], false);
        module.add_function("__entry_point", main_fn_type, None)
    }

    pub fn compile(
        ast: &AST,
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) {
        let compiler = ModuleCompiler {
            context,
            builder,
            module,
            variables: vec![HashMap::new()],
            entry_point_fn_value: Some(Self::create_main_function(context, module)),
            current_fn_value: None,
        };
        compiler.compile_root_nodes(ast.root_nodes)
    }
}
