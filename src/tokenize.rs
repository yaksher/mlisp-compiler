#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
pub enum Delim {
    Paren,
    Bracket,
    Brace,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
pub enum Keyword {
    Let,
    For,
    In,
    Do,
    If,
    And,
    Or,
    Assign,
}

impl TryFrom<&[u8]> for Keyword {
    type Error = ();
    fn try_from(input: &[u8]) -> Result<Self, Self::Error> {
        match input {
            b"let" => Ok(Keyword::Let),
            b"for" => Ok(Keyword::For),
            b"in" => Ok(Keyword::In),
            b"do" => Ok(Keyword::Do),
            b"if" => Ok(Keyword::If),
            b"and" => Ok(Keyword::And),
            b"or" => Ok(Keyword::Or),
            b"=" => Ok(Keyword::Assign),
            _ => Err(()),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum Literal {
    Int(i64),
    String(String),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum TokenTree {
    Delim(Delim, Vec<TokenTree>),
    Literal(Literal),
    Ident(String),
    Keyword(Keyword),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
enum Class {
    Whitespace,
    Digit,
    Symbol,
    Quote,
    Open(Delim),
    Close(Delim),
    Eof,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
enum State {
    Ident,
    Number,
    String,
    Whitespace,
    Delim(Delim),
}

impl State {
    fn after_whitespace(c: Class) -> Option<(State, usize)> {
        Some(match c {
            Class::Digit => (State::Number, 0),
            Class::Symbol => (State::Ident, 0),
            Class::Quote => (State::String, 1),
            Class::Open(del) => (State::Delim(del), 1),
            Class::Close(_) | Class::Eof => return None,
            Class::Whitespace => unreachable!(),
        })
    }
}

impl Class {
    fn of(c: Option<u8>) -> Class {
        let Some(c) = c else {
            return Class::Eof;
        };
        match c {
            b' ' | b'\t' | b'\n' => Class::Whitespace,
            b'0'..=b'9' => Class::Digit,
            b'"' => Class::Quote,
            b'(' => Class::Open(Delim::Paren),
            b'[' => Class::Open(Delim::Bracket),
            b'{' => Class::Open(Delim::Brace),
            b')' => Class::Close(Delim::Paren),
            b']' => Class::Close(Delim::Bracket),
            b'}' => Class::Close(Delim::Brace),
            _ => Class::Symbol,
        }
    }
}

fn escaped(c: u8) -> u8 {
    match c {
        b'n' => b'\n',
        b'r' => b'\r',
        b't' => b'\t',
        _ => c,
    }
}

fn token(input: &[u8]) -> Option<(TokenTree, usize)> {
    use Class as C;
    use State as S;
    let mut state = State::Whitespace;
    let mut i = 0usize;
    let mut buffer = Vec::new();
    let mut toks = Vec::new();
    let mut escape = false;
    loop {
        match (state, Class::of(input.get(i).copied())) {
            (S::Whitespace, C::Whitespace) => {
                i += 1;
            }
            (S::Whitespace, c) => {
                let (next_state, skip) = S::after_whitespace(c)?;
                state = next_state;
                i += skip;
            }
            (S::Number, C::Digit) => {
                buffer.push(*input.get(i)?);
                i += 1;
            }
            (S::Number, C::Whitespace | C::Open(_) | C::Close(_) | C::Eof) => {
                break;
            }
            (S::Number, C::Quote | C::Symbol) => {
                return None;
            }
            (S::Ident, C::Digit) if buffer == b"-" => {
                state = S::Number;
                buffer.push(*input.get(i)?);
                i += 1;
            }
            (S::Ident, C::Symbol | C::Digit) => {
                buffer.push(*input.get(i)?);
                i += 1;
            }
            (S::Ident, _) => {
                break;
            }
            (S::String, _) if escape => {
                buffer.push(escaped(*input.get(i)?));
                i += 1;
                escape = false;
            }
            (S::String, C::Quote) => {
                i += 1;
                break;
            }
            (S::String, _) if *input.get(i)? == b'\\' => {
                escape = true;
                i += 1;
            }
            (S::String, _) => {
                buffer.push(*input.get(i)?);
                i += 1;
            }
            (S::Delim(d), C::Close(d2)) if d == d2 => {
                i += 1;
                break;
            }
            (S::Delim(_), C::Whitespace) => {
                i += 1;
            }
            (S::Delim(_), _) => {
                let (tok, skip) = token(input.get(i..)?)?;
                toks.push(tok);
                i += skip;
            }
        }
    }
    let tok = match state {
        S::String => TokenTree::Literal(Literal::String(String::from_utf8(buffer).unwrap())),
        S::Number => {
            let num = std::str::from_utf8(&buffer).unwrap().parse().unwrap();
            TokenTree::Literal(Literal::Int(num))
        }
        S::Ident => {
            if let Ok(kw) = Keyword::try_from(buffer.as_slice()) {
                TokenTree::Keyword(kw)
            } else {
                TokenTree::Ident(String::from_utf8(buffer).unwrap())
            }
        }
        S::Delim(del) => TokenTree::Delim(del, toks),
        S::Whitespace => return None,
    };
    Some((tok, i))
}

pub fn tokenize(input: &[u8]) -> Vec<TokenTree> {
    let mut toks = Vec::new();
    let mut i = 0;
    while i < input.len() {
        let Some((tok, skip)) = token(&input[i..]) else {
            panic!(
                "Tokenization of `{}` failed at {i}. Read: {toks:?}",
                std::str::from_utf8(input).unwrap()
            );
        };
        toks.push(tok);
        i += skip;
    }
    toks
}
