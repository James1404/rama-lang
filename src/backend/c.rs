use crate::{
    metadata::Metadata,
    rair::{BinOp, CFG, Func, Loc, Operand, Place, RIL, RValue, Statement, Terminator, UnOp},
    types::{FloatKind, IntSize, Type, TypeID},
};
use core::fmt;
use log::error;
use std::{collections::HashSet, fmt::Write, io, path::Path};

type Result<T> = std::result::Result<T, fmt::Error>;

struct CodeBuilder<'a> {
    ril: RIL<'a>,
    code: String,
}

impl<'a> CodeBuilder<'a> {
    fn new(ril: RIL<'a>) -> Self {
        Self {
            ril,
            code: String::new(),
        }
    }

    fn loc(loc: Loc) -> String {
        format!("bb{}", loc.0)
    }

    fn place(place: Place) -> String {
        format!("_{}", place.0)
    }

    fn field(idx: usize) -> String {
        format!("field{}", idx)
    }

    fn convert_type(&self, ty: TypeID) -> String {
        match self.ril.ctx.get(ty) {
            Type::Void => "void".to_owned(),
            Type::Unit => "i32".to_owned(),
            Type::Bool => "bool".to_owned(),
            Type::Int { size, signed } => match (signed, size) {
                (true, IntSize::Bits8) => "i8".to_owned(),
                (true, IntSize::Bits16) => "i16".to_owned(),
                (true, IntSize::Bits32) => "i32".to_owned(),
                (true, IntSize::Bits64) => "i64".to_owned(),
                (true, IntSize::BitsPtr) => "isize".to_owned(),
                (false, IntSize::Bits8) => "u8".to_owned(),
                (false, IntSize::Bits16) => "u16".to_owned(),
                (false, IntSize::Bits32) => "u32".to_owned(),
                (false, IntSize::Bits64) => "u64".to_owned(),
                (false, IntSize::BitsPtr) => "usize".to_owned(),
            },
            Type::Float(FloatKind::F32) => "f32".to_owned(),
            Type::Float(FloatKind::F64) => "f64".to_owned(),
            Type::Ptr(inner) => format!("{}*", self.convert_type(inner)),
            _ => panic!("{}", self.ril.ctx.display(ty)),
        }
    }

    fn include_file(&mut self, file: &str) -> Result<()> {
        writeln!(self.code, "#include \"{}\"", file)
    }

    fn include_std(&mut self, file: &str) -> Result<()> {
        writeln!(self.code, "#include <{}>", file)
    }

    fn define(&mut self, name: &str, value: &str) -> Result<()> {
        writeln!(self.code, "#define {} {}", name, value)
    }

    fn typedef(&mut self, name: &str, ty: &str) -> Result<()> {
        writeln!(self.code, "typedef {} {};", ty, name)
    }

    fn eval_operand(&mut self, operand: &Operand<'a>) -> String {
        match operand {
            Operand::Const(kind) => match kind {
                crate::rair::ConstKind::Float(val) => format!("{val}"),
                crate::rair::ConstKind::Integer(val) => format!("{val}"),
                crate::rair::ConstKind::String(val) => format!("{val}"),
                crate::rair::ConstKind::True => format!("true"),
                crate::rair::ConstKind::False => format!("false"),
                crate::rair::ConstKind::Unit => format!("0"),
            },
            Operand::Copy(place) => Self::place(*place),
        }
    }

    fn eval_binop(&mut self, op: BinOp) -> &'static str {
        match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
        }
    }

    fn eval_unop(&mut self, op: UnOp) -> &'static str {
        match op {
            UnOp::Not => "!",
            UnOp::Negate => "-",
        }
    }

    fn eval_rvalue(&mut self, rvalue: &RValue<'a>) -> Result<String> {
        match rvalue {
            RValue::Use(operand) => Ok(format!("{}", self.eval_operand(operand))),
            RValue::BinaryOp(op, lhs, rhs) => Ok(format!(
                "{} {} {}",
                self.eval_operand(lhs),
                self.eval_binop(*op),
                self.eval_operand(rhs)
            )),
            RValue::UnaryOp(op, value) => Ok(format!(
                "{}{}",
                self.eval_unop(*op),
                self.eval_operand(value)
            )),
            RValue::Cast(value, ty) => Ok(format!(
                "({}){}",
                self.convert_type(*ty),
                self.eval_operand(value)
            )),
            RValue::Ref(place) => todo!(),
            RValue::Call(func, args) => {
                let mut buf = format!(
                    "{}(",
                    match self.ril.get_func(*func) {
                        Func::Decl { name, .. } => name,
                        Func::Extern { name, .. } => name,
                    }
                );
                let mut iter = args.iter().peekable();
                while let Some(arg) = iter.next() {
                    write!(buf, "{}", self.eval_operand(arg))?;

                    if iter.peek().is_some() {
                        write!(buf, ", ")?;
                    }
                }
                write!(buf, ")")?;

                Ok(buf)
            }
            RValue::Aggregate(kind) => todo!(),
            RValue::Len(place) => todo!(),
        }
    }

    fn eval_terminator(&mut self, terminator: &Terminator<'a>) -> Result<()> {
        match terminator {
            Terminator::Goto(loc) => writeln!(self.code, "goto {};", Self::loc(*loc)),
            Terminator::If { cond, t, f } => writeln!(
                self.code,
                "if ({}) goto {}; else goto {};",
                Self::place(*cond),
                Self::loc(*t),
                Self::loc(*f)
            ),
            Terminator::ReturnNone => writeln!(self.code, "return;"),
            Terminator::Return(operand) => {
                let operand = self.eval_operand(operand);
                writeln!(self.code, "return {};", operand)
            }
        }
    }

    fn newline(&mut self) -> Result<()> {
        writeln!(self.code, "")
    }

    fn eval_cfg(&mut self, cfg: CFG<'a>) -> Result<()> {
        let mut defined = HashSet::<Place>::new();

        for (loc, bb) in cfg.blocks.iter_enumerated() {
            writeln!(self.code, "{}:", Self::loc(loc))?;
            writeln!(self.code, "\t{{}}")?;

            for stmt in &bb.statements {
                write!(self.code, "\t")?;
                match stmt {
                    Statement::Assign(place, rvalue) => {
                        let rvalue = self.eval_rvalue(rvalue)?;
                        if !defined.contains(place) {
                            write!(self.code, "{} ", self.convert_type(place.2))?;
                        }

                        defined.insert(*place);
                        writeln!(self.code, "{} = {};", Self::place(*place), rvalue)?;
                    }
                }
            }

            write!(self.code, "\t")?;
            self.eval_terminator(&bb.terminator)?;
        }

        Ok(())
    }

    fn build(mut self) -> Result<String> {
        self.include_std("assert.h")?;
        self.include_std("stdio.h")?;
        self.include_std("stdlib.h")?;
        self.include_std("stdbool.h")?;
        self.include_std("stdint.h")?;
        self.include_std("string.h")?;

        self.newline()?;

        self.typedef("f32", "float")?;
        self.typedef("f64", "long double")?;

        self.newline()?;

        self.typedef("u8", "unsigned char")?;
        self.typedef("u16", "unsigned short int")?;
        self.typedef("u32", "unsigned long int")?;
        self.typedef("u64", "unsigned long long int")?;
        self.typedef("usize", "unsigned long long int")?;

        self.newline()?;

        self.typedef("i8", "char")?;
        self.typedef("i16", "short int")?;
        self.typedef("i32", "long int")?;
        self.typedef("i64", "long long int")?;
        self.typedef("isize", "long long int")?;

        self.newline()?;

        self.define("TODO", "i32")?;

        self.newline()?;

        //
        // forward declerations
        //

        for func in self.ril.clone().funcs {
            match func {
                Func::Decl { name, cfg, return_ty, params } => {
                    let return_ty = self.convert_type(return_ty);
                    write!(self.code, "{} {}(", return_ty, name)?;

                    let mut iter = params.iter().enumerate().peekable();
                    while let Some((idx, param)) = iter.next() {
                        write!(
                            self.code,
                            "{} {}",
                            self.convert_type(param.ty),
                            Self::place(param.place)
                        )?;

                        if iter.peek().is_some() {
                            write!(self.code, ", ")?;
                        }
                    }

                    writeln!(self.code, ");")?;
                },
                Func::Extern { name, return_ty, params } => {
                    let return_ty = self.convert_type(return_ty);
                    write!(self.code, "extern {} {}(", return_ty, name)?;

                    let mut iter = params.iter().enumerate().peekable();
                    while let Some((idx, param)) = iter.next() {
                        write!(
                            self.code,
                            "{} {}",
                            self.convert_type(param.ty),
                            format!("_{idx}"),
                        )?;

                        if iter.peek().is_some() {
                            write!(self.code, ", ")?;
                        }
                    }

                    writeln!(self.code, ");")?;
                }
            }
        }

        self.newline()?;

        for func in self.ril.clone().funcs {
            match func {
                Func::Decl { name, cfg, return_ty, params } => {
                    let return_ty = self.convert_type(return_ty);
                    write!(self.code, "{} {}(", return_ty, name)?;

                    let mut iter = params.iter().enumerate().peekable();
                    while let Some((idx, param)) = iter.next() {
                        write!(
                            self.code,
                            "{} {}",
                            self.convert_type(param.ty),
                            Self::place(param.place)
                        )?;

                        if iter.peek().is_some() {
                            write!(self.code, ", ")?;
                        }
                    }

                    writeln!(self.code, ") {{")?;
                    self.eval_cfg(cfg)?;
                    writeln!(self.code, "}}")?;

                }
                _ => {},
            }
        }

        Ok(self.code)
    }
}

pub fn compile<'a>(rair: RIL<'a>, metadata: Metadata<'a>) -> io::Result<()> {
    let code = CodeBuilder::new(rair);
    let code = code.build().unwrap();

    println!("{}", code);

    let path = format!("build/{}.c", metadata.name);
    let path = Path::new(&path);

    let dir = path.parent().unwrap();
    std::fs::create_dir_all(dir)?;

    let mut file = std::fs::File::create(path)?;
    io::Write::write_all(&mut file, code.as_bytes())?;

    match std::process::Command::new("cc")
        .arg(format!("-o build/{}.exe", metadata.name))
        .arg(path)
        .spawn()
    {
        Ok(_) => {}
        Err(err) => error!("Failed to compile c code"),
    }

    Ok(())
}
