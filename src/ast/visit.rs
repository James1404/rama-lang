#![allow(dead_code, unused)]

use super::*;

pub trait Visit<'a> {
    fn visit_ident(&mut self, value: Ident<'a>) {
        visit_ident(self, value);
    }

    fn visit_literal_record_field(&mut self, value: LiteralRecordField<'a>) {
        visit_literal_record_field(self, value);
    }

    fn visit_record_field(&mut self, value: RecordField<'a>) {
        visit_record_field(self, value);
    }

    fn visit_enum_variant(&mut self, value: EnumVariant<'a>) {
        visit_enum_variant(self, value);
    }

    fn visit_value_float(&mut self, value: &'a str) {
        visit_value_float(self, value);
    }

    fn visit_value_int(&mut self, value: &'a str) {
        visit_value_int(self, value);
    }

    fn visit_value_string(&mut self, value: &'a str) {
        visit_value_string(self, value);
    }

    fn visit_value_bool(&mut self, value: bool) {
        visit_value_bool(self, value);
    }

    fn visit_value(&mut self, value: Value<'a>) {
        visit_value(self, value);
    }

    fn visit_expr(&mut self, value: Expr<'a>) {
        visit_expr(self, value);
    }

    fn visit_type(&mut self, value: Type<'a>) {
        visit_type(self, value);
    }

    fn visit_block(&mut self, value: Block<'a>) {
        visit_block(self, value);
    }

    fn visit_const_decl(&mut self, value: ConstDecl<'a>) {
        visit_const_decl(self, value);
    }

    fn visit_let_decl(&mut self, value: LetDecl<'a>) {
        visit_let_decl(self, value);
    }

    fn visit_param(&mut self, value: Param<'a>) {
        visit_param(self, value);
    }

    fn visit_extern_fn(&mut self, value: ExternFn<'a>) {
        visit_extern_fn(self, value);
    }

    fn visit_fn(&mut self, value: Fn<'a>) {
        visit_fn(self, value);
    }

    fn visit_if(&mut self, value: If<'a>) {
        visit_if(self, value);
    }

    fn visit_match_branch(&mut self, value: MatchBranch<'a>) {
        visit_match_branch(self, value);
    }

    fn visit_match(&mut self, value: Match<'a>) {
        visit_match(self, value);
    }

    fn visit_import(&mut self, value: Import<'a>) {
        visit_import(self, value);
    }

    fn visit_statement(&mut self, value: Statement<'a>) {
        visit_statement(self, value);
    }

    fn visit_top_level_statement(&mut self, value: TopLevelStatement<'a>) {
        visit_top_level_statement(self, value);
    }

    fn visit_ast(&mut self, value: AST<'a>) {
        visit_ast(self, value);
    }
}

pub fn visit_ident<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Ident<'a>) {}

pub fn visit_literal_record_field<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: LiteralRecordField<'a>) {
    v.visit_ident(value.ident);
    v.visit_expr(value.value);
}

pub fn visit_record_field<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: RecordField<'a>) {
    v.visit_ident(value.ident);
    v.visit_type(value.ty);
}

pub fn visit_enum_variant<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: EnumVariant<'a>) {
    v.visit_ident(value.ident);
    v.visit_type(value.ty);
}


pub fn visit_value_float<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: &'a str) {
}

pub fn visit_value_int<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: &'a str) {
}

pub fn visit_value_string<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: &'a str) {
}

pub fn visit_value_bool<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: bool) {
}

pub fn visit_value<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Value<'a>) {
    match value {
        Value::Ident(ident) => v.visit_ident(ident),
        Value::Float(v) => v.visit_value_float(v),
        Value::Int(v) => v.visit_value_int(v),
        Value::String(v) => v.visit_value_string(v),
        Value::Bool(v) => v.visit_value_bool(v),
        Value::Record { fields } => {
            for field in fields {
                v.visit_literal_record_field(field);
            }
        }
        Value::Call { func, args } => {}
        Value::FieldAccess { value, field } => todo!(),
        Value::Index { value, index } => todo!(),
        Value::Ref(expr) => todo!(),
        Value::Deref(expr) => todo!(),
    }
}

pub fn visit_expr<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Expr<'a>) {}
pub fn visit_type<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Type<'a>) {}
pub fn visit_block<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Block<'a>) {}
pub fn visit_const_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: ConstDecl<'a>) {}
pub fn visit_let_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: LetDecl<'a>) {}

pub fn visit_param<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Param<'a>) {
    v.visit_ident(value.ident);
    v.visit_type(value.ty);
}

pub fn visit_extern_fn<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: ExternFn<'a>) {
    v.visit_ident(value.ident);

    for param in value.params {
        v.visit_param(param);
    }

    if let Some(ret) = value.ret {
        v.visit_type(ret);
    }
}

pub fn visit_fn<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Fn<'a>) {
    v.visit_ident(value.ident);

    for param in value.params {
        v.visit_param(param);
    }

    if let Some(ret) = value.ret {
        v.visit_type(ret);
    }

    v.visit_block(value.block);
}

pub fn visit_if<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: If<'a>) {
    v.visit_expr(value.expr);
    v.visit_block(value.then);

    if let Some(otherwise) = value.otherwise {
        v.visit_block(otherwise);
    }
}

pub fn visit_match_branch<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: MatchBranch<'a>) {
    v.visit_expr(value.pattern);
    v.visit_expr(value.value);
}

pub fn visit_match<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Match<'a>) {
    v.visit_value(value.value);

    for branch in value.branches {
        v.visit_match_branch(branch);
    }
}

pub fn visit_import<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Import<'a>) {}

pub fn visit_statement<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: Statement<'a>) {
    match value {
        Statement::ConstDecl(const_decl) => v.visit_const_decl(const_decl),
        Statement::LetDecl(let_decl) => v.visit_let_decl(let_decl),
        Statement::Expr(expr) => v.visit_expr(expr),
        Statement::Return(expr) => v.visit_expr(expr),
        Statement::ReturnNone => {}
    }
}

pub fn visit_top_level_statement<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: TopLevelStatement<'a>) {
    match value {
        TopLevelStatement::ConstDecl(const_decl) => v.visit_const_decl(const_decl),
        TopLevelStatement::LetDecl(let_decl) => v.visit_let_decl(let_decl),
        TopLevelStatement::Type { ident, inner } => v.visit_type(inner),
        TopLevelStatement::ExternFn(extern_fn) => v.visit_extern_fn(extern_fn),
        TopLevelStatement::Fn(v) => v.visit_fn(v),
        TopLevelStatement::Import(import) => v.visit_import(import),
    }
}

pub fn visit_ast<'a, V: Visit<'a> + ?Sized>(v: &mut V, value: AST<'a>) {
    for stmt in value.statements {
        v.visit_top_level_statement(stmt);
    }
}
