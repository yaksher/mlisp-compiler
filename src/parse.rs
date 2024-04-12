#![allow(dead_code)]
use std::{
    collections::HashMap,
    io::{self, Write},
};

use super::tokenize::{Delim as Del, Keyword as KW, Literal as TokLit, TokenTree as TT};

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
#[repr(u8)]
pub enum Builtin {
    // general
    TypeOf,
    Repr,
    Not, // not 0, [], "", none -> 1, everything else is 0
    // structural equality
    Eq,
    Neq,
    // lexicographic for lists and strings, numeric for integers, none < everything else, functions and streams errors
    Lt,
    Gt,
    Le,
    Ge,
    // integer arithmetic
    Add,
    Sub,
    Mul,
    Div,
    // list/string operations
    Len,
    Push,
    Pop,
    Get,
    Set,
    // string operations
    Int,
    Split,
    Trim,
    Join,
    // stream operations
    Open,
    Read,
    Write,
    Close,
}

impl TryFrom<&str> for Builtin {
    type Error = ();
    fn try_from(input: &str) -> Result<Self, Self::Error> {
        match input {
            "typeof" => Ok(Builtin::TypeOf),
            "repr" => Ok(Builtin::Repr),
            "not" => Ok(Builtin::Not),

            "==" => Ok(Builtin::Eq),
            "!=" => Ok(Builtin::Neq),

            "<" => Ok(Builtin::Lt),
            ">" => Ok(Builtin::Gt),
            "<=" => Ok(Builtin::Le),
            ">=" => Ok(Builtin::Ge),

            "+" => Ok(Builtin::Add),
            "-" => Ok(Builtin::Sub),
            "*" => Ok(Builtin::Mul),
            "/" => Ok(Builtin::Div),

            "len" => Ok(Builtin::Len),
            "push" => Ok(Builtin::Push),
            "pop" => Ok(Builtin::Pop),
            "get" => Ok(Builtin::Get),
            "set" => Ok(Builtin::Set),

            "int" => Ok(Builtin::Int),
            "split" => Ok(Builtin::Split),
            "trim" => Ok(Builtin::Trim),
            "join" => Ok(Builtin::Join),

            "open" => Ok(Builtin::Open),
            "read" => Ok(Builtin::Read),
            "write" => Ok(Builtin::Write),
            "close" => Ok(Builtin::Close),
            _ => Err(()),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum Literal {
    None,
    Int(i64),
    String(String),
}

impl From<TokLit> for Literal {
    fn from(lit: TokLit) -> Self {
        match lit {
            TokLit::Int(i) => Literal::Int(i),
            TokLit::String(s) => Literal::String(s),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum Decl<Ident> {
    Fn(Ident, FnArgs<Ident>, Expr<Ident>),
    Bind(Ident, Expr<Ident>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum FnArgs<Ident> {
    One(Ident),
    List(Vec<Ident>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
#[repr(u8)]
pub enum Expr<Ident> {
    For(Ident, Box<Expr<Ident>>, Box<Expr<Ident>>),
    Bind(Ident, Box<Expr<Ident>>, Box<Expr<Ident>>),
    Assign(Ident, Box<Expr<Ident>>),
    If(Box<Expr<Ident>>, Box<Expr<Ident>>, Box<Expr<Ident>>),
    Call(Box<Expr<Ident>>, Vec<Expr<Ident>>),
    And(Vec<Expr<Ident>>),
    Or(Vec<Expr<Ident>>),
    Sequence(Vec<Expr<Ident>>),
    List(Vec<Expr<Ident>>),
    Literal(Literal),
    Builtin(Builtin),
    Ident(Ident),
}

pub type Ast<Ident = String> = Vec<Decl<Ident>>;

pub fn parse_exprs(toks: &[TT]) -> Option<Vec<Expr<String>>> {
    let mut exprs = Vec::new();
    let mut i = 0;
    while i < toks.len() {
        let (expr, skip) = parse_expr(toks.get(i..)?)?;
        exprs.push(expr);
        i += skip;
    }
    Some(exprs)
}

pub fn parse_expr(toks: &[TT]) -> Option<(Expr<String>, usize)> {
    let out = match toks {
        [TT::Keyword(KW::For), TT::Ident(name), TT::Keyword(KW::In), rest @ ..] => {
            let (iter, skip_iter) = parse_expr(rest)?;
            if !matches!(rest.get(skip_iter), Some(TT::Keyword(KW::Do))) {
                return None;
            }
            let (body, skip_body) = parse_expr(rest.get(skip_iter + 1..)?)?;
            (
                Expr::For(name.clone(), Box::new(iter), Box::new(body)),
                4 + skip_iter + skip_body,
            )
        }
        [TT::Keyword(KW::Let), TT::Ident(name), TT::Keyword(KW::Assign), rest @ ..] => {
            let (val, skip_val) = parse_expr(rest)?;
            if !matches!(rest.get(skip_val), Some(TT::Keyword(KW::In))) {
                return None;
            }
            let (body, skip_body) = parse_expr(rest.get(skip_val + 1..)?)?;
            (
                Expr::Bind(name.clone(), Box::new(val), Box::new(body)),
                4 + skip_val + skip_body,
            )
        }
        [TT::Keyword(KW::If), rest @ ..] => {
            let (cond, skip_cond) = parse_expr(rest)?;
            let (then, skip_then) = parse_expr(rest.get(skip_cond..)?)?;
            let (els, skip_els) = parse_expr(rest.get(skip_cond + skip_then..)?)?;
            (
                Expr::If(Box::new(cond), Box::new(then), Box::new(els)),
                1 + skip_cond + skip_then + skip_els,
            )
        }
        [TT::Delim(Del::Paren, toks), ..] => match toks.as_slice() {
            [] => (Expr::Literal(Literal::None), 1),
            [TT::Ident(name), TT::Keyword(KW::Assign), ..] => {
                let (val, skip_val) = parse_expr(toks.get(2..)?)?;
                (Expr::Assign(name.clone(), Box::new(val)), 2 + skip_val)
            }
            [TT::Keyword(KW::And), ..] => {
                let exprs = parse_exprs(toks.get(1..)?)?;
                (Expr::And(exprs), 1)
            }
            [TT::Keyword(KW::Or), ..] => {
                let exprs = parse_exprs(toks.get(1..)?)?;
                (Expr::Or(exprs), 1)
            }
            _ => {
                let (fun, skip_fun) = parse_expr(&toks)?;
                let args = parse_exprs(toks.get(skip_fun..)?)?;
                (Expr::Call(Box::new(fun), args), 1)
            }
        },
        [TT::Delim(del, toks), ..] => {
            let exprs = parse_exprs(toks)?;
            match del {
                Del::Bracket => (Expr::List(exprs), 1),
                Del::Brace if exprs.len() == 0 => (Expr::Literal(Literal::None), 1),
                Del::Brace => (Expr::Sequence(exprs), 1),
                Del::Paren => unreachable!("covered by previous cases"),
            }
        }
        [TT::Ident(name), ..] => (
            match Builtin::try_from(name.as_str()) {
                Ok(b) => Expr::Builtin(b),
                Err(()) => Expr::Ident(name.clone()),
            },
            1,
        ),
        [TT::Literal(lit), ..] => (Expr::Literal(lit.clone().into()), 1),
        _ => return None,
    };
    Some(out)
}

pub fn parse_args(tok: &TT) -> Option<FnArgs<String>> {
    match tok {
        TT::Ident(name) => Some(FnArgs::One(name.clone())),
        TT::Delim(Del::Bracket, args) => {
            let mut names = Vec::new();
            for arg in args {
                if let TT::Ident(name) = arg {
                    names.push(name.clone());
                } else {
                    return None;
                }
            }
            Some(FnArgs::List(names))
        }
        _ => None,
    }
}

pub fn parse_decl(input: &[TT]) -> Option<(Decl<String>, usize)> {
    match input {
        [TT::Keyword(KW::Let), TT::Ident(name), TT::Keyword(KW::Assign), rest @ ..] => {
            let (expr, skip) = parse_expr(rest)?;
            Some((Decl::Bind(name.clone(), expr), skip + 3))
        }
        [TT::Keyword(KW::Let), TT::Ident(name), args, TT::Keyword(KW::Assign), rest @ ..] => {
            let args = parse_args(args)?;
            let (expr, skip) = parse_expr(rest)?;
            Some((Decl::Fn(name.clone(), args, expr), skip + 4))
        }
        _ => None,
    }
}

pub fn parse(input: &[TT]) -> Ast {
    let mut decls = Vec::new();
    let mut i = 0;
    while i < input.len() {
        let Some((decl, n)) = parse_decl(&input[i..]) else {
            panic!("parse error {}, {:#?}", i, decls);
        };
        decls.push(decl);
        i += n;
    }
    decls
}

fn ident_id<T>(h: &mut HashMap<String, T>, name: String) -> T
where
    T: Copy + TryFrom<usize>,
    <T as TryFrom<usize>>::Error: std::fmt::Debug,
{
    if let Some(id) = h.get(&name) {
        *id
    } else {
        let id = h.len().try_into().expect("too many identifiers");
        h.insert(name, id);
        id
    }
}

impl Expr<String> {
    fn enumerate_idents<T>(self, h: &mut HashMap<String, T>) -> Expr<T>
    where
        T: Copy + TryFrom<usize>,
        <T as TryFrom<usize>>::Error: std::fmt::Debug,
    {
        match self {
            Expr::For(name, iter, body) => {
                let id = ident_id(h, name);
                Expr::For(
                    id,
                    iter.enumerate_idents(h).into(),
                    body.enumerate_idents(h).into(),
                )
            }
            Expr::Bind(name, val, body) => {
                let id = ident_id(h, name);
                Expr::Bind(
                    id,
                    val.enumerate_idents(h).into(),
                    body.enumerate_idents(h).into(),
                )
            }
            Expr::Assign(name, val) => {
                let id = ident_id(h, name);
                Expr::Assign(id, val.enumerate_idents(h).into())
            }
            Expr::If(cond, then, els) => Expr::If(
                cond.enumerate_idents(h).into(),
                then.enumerate_idents(h).into(),
                els.enumerate_idents(h).into(),
            ),
            Expr::Call(fun, args) => Expr::Call(
                fun.enumerate_idents(h).into(),
                args.into_iter()
                    .map(|arg| arg.enumerate_idents(h).into())
                    .collect(),
            ),
            Expr::And(exprs) => Expr::And(
                exprs
                    .into_iter()
                    .map(|expr| expr.enumerate_idents(h))
                    .collect(),
            ),
            Expr::Or(exprs) => Expr::Or(
                exprs
                    .into_iter()
                    .map(|expr| expr.enumerate_idents(h))
                    .collect(),
            ),
            Expr::Sequence(exprs) => Expr::Sequence(
                exprs
                    .into_iter()
                    .map(|expr| expr.enumerate_idents(h))
                    .collect(),
            ),
            Expr::List(exprs) => Expr::List(
                exprs
                    .into_iter()
                    .map(|expr| expr.enumerate_idents(h))
                    .collect(),
            ),
            Expr::Literal(lit) => Expr::Literal(lit),
            Expr::Builtin(b) => Expr::Builtin(b),
            Expr::Ident(name) => Expr::Ident(ident_id(h, name)),
        }
    }
}

pub fn enumerate_idents<T>(ast: Ast<String>) -> Ast<T>
where
    T: Copy + TryFrom<usize>,
    <T as TryFrom<usize>>::Error: std::fmt::Debug,
{
    let mut glob_ids = HashMap::new();
    ast.iter().for_each(|decl| {
        let name = match decl {
            Decl::Fn(name, ..) => name,
            Decl::Bind(name, _) => name,
        };
        let _ = ident_id(&mut glob_ids, name.clone());
    });
    ast.into_iter()
        .map(|decl| match decl {
            Decl::Fn(name, args, body) => {
                let id = ident_id(&mut glob_ids, name);
                let mut ids = glob_ids.clone();
                let args = match args {
                    FnArgs::One(name) => FnArgs::One(ident_id(&mut ids, name)),
                    FnArgs::List(names) => FnArgs::List(
                        names
                            .into_iter()
                            .map(|name| ident_id(&mut ids, name))
                            .collect(),
                    ),
                };
                Decl::Fn(id, args, body.enumerate_idents(&mut ids))
            }
            Decl::Bind(name, body) => {
                let id = ident_id(&mut glob_ids, name);
                let mut ids = glob_ids.clone();
                Decl::Bind(id, body.enumerate_idents(&mut ids))
            }
        })
        .collect()
}

pub trait Serialize {
    fn serialize<'a, T: Write>(&self, out: &'a mut T) -> io::Result<()>;
}

macro_rules! impl_serialize_int {
    ($($t:ty),*) => {
        $(
            impl Serialize for $t {
                fn serialize<'a, T: Write>(&self, out: &'a mut T) -> io::Result<()> {
                    out.write_all(&self.to_le_bytes())
                }
            }
        )*
    };
}

impl_serialize_int! {u8, u16, u32, u64, usize, i8, i16, i32, i64, isize}

impl<U: Serialize> Serialize for Vec<U> {
    fn serialize<'a, T: Write>(&self, out: &'a mut T) -> io::Result<()> {
        let len = self.len();
        let len: u16 = len.try_into().expect("too many elements");
        len.serialize(out)?;
        for item in self {
            item.serialize(out)?;
        }
        Ok(())
    }
}

impl Serialize for Literal {
    fn serialize<'a, T: Write>(&self, out: &'a mut T) -> io::Result<()> {
        match self {
            Literal::None => 0u8.serialize(out),
            Literal::Int(i) => {
                1u8.serialize(out)?;
                i.serialize(out)
            }
            Literal::String(s) => {
                2u8.serialize(out)?;
                s.len().serialize(out)?;
                out.write_all(s.as_bytes())
            }
        }
    }
}

impl Serialize for Builtin {
    fn serialize<'a, T: Write>(&self, out: &'a mut T) -> io::Result<()> {
        (*self as u8).serialize(out)?;
        Ok(())
    }
}

impl<U: Serialize> Serialize for FnArgs<U> {
    fn serialize<'a, T: Write>(&self, out: &'a mut T) -> io::Result<()> {
        match self {
            FnArgs::One(id) => {
                0u8.serialize(out)?;
                id.serialize(out)
            }
            FnArgs::List(ids) => {
                let len = ids.len();
                let len: u8 = (len < 255).then(|| len as u8).expect("too many arguments");
                (len + 1).serialize(out)?;
                for id in ids.iter() {
                    id.serialize(out)?;
                }
                Ok(())
            }
        }
    }
}

impl<U: Serialize> Serialize for Expr<U> {
    fn serialize<'a, T: Write>(&self, out: &'a mut T) -> io::Result<()> {
        match self {
            Expr::For(id, iter, body) => {
                0u8.serialize(out)?;
                id.serialize(out)?;
                iter.serialize(out)?;
                body.serialize(out)
            }
            Expr::Bind(id, val, body) => {
                1u8.serialize(out)?;
                id.serialize(out)?;
                val.serialize(out)?;
                body.serialize(out)
            }
            Expr::Assign(id, val) => {
                2u8.serialize(out)?;
                id.serialize(out)?;
                val.serialize(out)
            }
            Expr::If(cond, then, els) => {
                3u8.serialize(out)?;
                cond.serialize(out)?;
                then.serialize(out)?;
                els.serialize(out)
            }
            Expr::Call(fun, args) => {
                4u8.serialize(out)?;
                fun.serialize(out)?;
                args.serialize(out)
            }
            Expr::And(exprs) => {
                5u8.serialize(out)?;
                exprs.serialize(out)
            }
            Expr::Or(exprs) => {
                6u8.serialize(out)?;
                exprs.serialize(out)
            }
            Expr::Sequence(exprs) => {
                7u8.serialize(out)?;
                exprs.serialize(out)
            }
            Expr::List(exprs) => {
                8u8.serialize(out)?;
                exprs.serialize(out)
            }
            Expr::Literal(lit) => {
                9u8.serialize(out)?;
                lit.serialize(out)
            }
            Expr::Builtin(b) => {
                10u8.serialize(out)?;
                b.serialize(out)
            }
            Expr::Ident(id) => {
                11u8.serialize(out)?;
                id.serialize(out)
            }
        }
    }
}

impl<U: Serialize> Serialize for Decl<U> {
    fn serialize<'a, T: Write>(&self, out: &'a mut T) -> io::Result<()> {
        match self {
            Decl::Fn(id, args, body) => {
                0u8.serialize(out)?;
                id.serialize(out)?;
                args.serialize(out)?;
                body.serialize(out)
            }
            Decl::Bind(id, body) => {
                1u8.serialize(out)?;
                id.serialize(out)?;
                body.serialize(out)
            }
        }
    }
}
