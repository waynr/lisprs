use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::str::Chars;

type Error = Box<dyn std::error::Error>;
type Result<T> = std::result::Result<T, Error>;

// Abstract Syntax Tree
#[derive(Debug, PartialEq)]
enum Expr {
    Function(Function),
    List(Vec<Expr>),
    Atom(Atom),
    Empty,
}

impl Parse for Expr {
    fn parse(ts: &TokenStream) -> Result<Self> {
        let mut exprs = Vec::new();
        loop {
            if let Some(expr) = ts.step(|tt| match tt {
                TokenTree::Group(group) => match group.token_stream.peek(1) {
                    Some(TokenTree::Token(Token::Ident(_))) => Ok(Some(Expr::Function(
                        group.token_stream.parse::<Function>()?,
                    ))),
                    Some(TokenTree::Group(_)) => Ok(Some(Expr::Function(
                        group.token_stream.parse::<Function>()?,
                    ))),
                    _ => {
                        return Err(Error::from(
                            format!("unexpected token meow: {:?}", tt).to_string(),
                        ))
                    }
                },
                _ => Err(Error::from(
                    format!("unexpected token wut: {:?}", tt).to_string(),
                )),
            })? {
                exprs.push(expr);
            }
            {
                break;
            }
        }
        if exprs.len() == 0 {
            Ok(Expr::Empty)
        } else {
            Ok(Expr::List(exprs))
        }
    }
}

#[derive(Debug, PartialEq)]
struct Function {
    name: Atom,
    args: Vec<Expr>,
}

impl Parse for Function {
    fn parse(ts: &TokenStream) -> Result<Self> {
        let _ = ts.step(|tt| match tt {
            TokenTree::Token(Token::LeftParen) => Ok(Some(Expr::Empty)),
            _ => Err(Error::from(
                format!("invalid token '{:?}', expected left paren", tt).to_string(),
            )),
        });
        let name = match ts.step(|tt| match tt {
            TokenTree::Token(Token::Ident(s)) => Ok(Some(Atom::Symbol(s.clone()))),
            _ => Err(Error::from(
                format!("invalid token '{:?}', expected identifier", tt).to_string(),
            )),
        }) {
            Ok(Some(s)) => s,
            Ok(None) => {
                return Err(Error::from(
                    format!("expected function name, found nothing").to_string(),
                ))
            }
            Err(e) => return Err(e),
        };

        let mut args = Vec::new();
        loop {
            let result = ts.step(|tt| match tt {
                TokenTree::Group(group) => Ok(Some(Expr::Function(
                    group.token_stream.parse::<Function>()?,
                ))),
                TokenTree::Token(Token::Literal(Literal::UInt32(u))) => {
                    Ok(Some(Expr::Atom(Atom::UInt32(*u))))
                }
                TokenTree::Token(Token::RightParen) => Ok(None),
                _ => Err(Error::from(
                    format!("expected group or literal woof: {:?}", tt).to_string(),
                )),
            });
            match result {
                Ok(Some(Expr::Empty)) => continue,
                Ok(Some(e)) => args.push(e),
                Ok(None) => break,
                Err(e) => return Err(e),
            }
        }
        Ok(Self { name, args })
    }
}

#[derive(Debug, PartialEq)]
enum Atom {
    Symbol(String),
    UInt32(u32),
}

impl TryFrom<&Literal> for Atom {
    type Error = Error;

    fn try_from(l: &Literal) -> Result<Self> {
        match l {
            Literal::UInt32(i) => Ok(Atom::UInt32(*i)),
        }
    }
}

struct List {
    elements: Vec<Atom>,
}

trait Parse: Sized {
    fn parse(ts: &TokenStream) -> Result<Self>;
}

// input handling
#[derive(Debug, PartialEq)]
struct TokenStream {
    tokens: Vec<TokenTree>,
    index: Rc<RefCell<usize>>,
}

impl TokenStream {
    fn step<F, R>(&self, func: F) -> Result<Option<R>>
    where
        F: FnOnce(&TokenTree) -> Result<Option<R>>,
    {
        if let Some(&ref tt) = {
            let mut i = self.index.borrow_mut();
            let tt = self.tokens.get(*i);
            *i += 1;
            tt
        } {
            func(&tt)
        } else {
            Ok(None)
        }
    }

    fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    fn peek(&self, incr: usize) -> Option<&TokenTree> {
        self.tokens.get(*self.index.borrow() + incr)
    }
}

impl TryFrom<Rc<RefCell<Chars<'_>>>> for TokenStream {
    type Error = Error;

    fn try_from(cs: Rc<RefCell<Chars>>) -> Result<Self> {
        let tokens = RefCell::new(Vec::new());
        let sacc = Rc::new(RefCell::new(String::new()));

        let terminate_acc = || {
            let rc_sacc = sacc.clone();
            let mut refcell_sacc = rc_sacc.borrow_mut();
            if refcell_sacc.len() > 0 {
                tokens.borrow_mut().push(refcell_sacc.clone().into());
                *refcell_sacc = String::new();
            }
        };

        loop {
            let c: char;
            if let Some(ch) = cs.borrow_mut().next() {
                c = ch
            } else {
                if let Some(t) = tokens.borrow().last() {
                    match t {
                        TokenTree::Group(g) => match g.token_stream.tokens.last() {
                            Some(TokenTree::Token(t)) => match t {
                                Token::RightParen => (),
                                _ => {
                                    return Err(Error::from(
                                        format!("missing final parenthesis!").to_string(),
                                    ));
                                }
                            },
                            _ => {
                                return Err(Error::from(
                                    format!("missing final parenthesis!").to_string(),
                                ));
                            }
                        },
                        _ => unreachable!(),
                    }
                }
                break;
            }
            match c {
                '(' => {
                    terminate_acc();
                    let mut group = Group {
                        token_stream: TokenStream::try_from(cs.clone())?,
                    };
                    group
                        .token_stream
                        .tokens
                        .insert(0, TokenTree::Token(Token::LeftParen));
                    let tree = TokenTree::Group(group);
                    tokens.borrow_mut().push(tree);
                }
                ')' => {
                    terminate_acc();
                    tokens
                        .borrow_mut()
                        .push(TokenTree::Token(Token::RightParen));
                    break;
                }
                ' ' => {
                    terminate_acc();
                }
                _ if c.is_alphanumeric() | c.is_ascii_punctuation() => {
                    sacc.clone().borrow_mut().push(c)
                }
                _ => {
                    return Err(Error::from(
                        format!("unsupported character {}", c).to_string(),
                    ));
                }
            }
        }
        Ok(Self {
            tokens: tokens.take(),
            index: Rc::new(RefCell::new(0)),
        })
    }
}

impl From<Vec<TokenTree>> for TokenStream {
    fn from(tts: Vec<TokenTree>) -> Self {
        Self {
            tokens: tts,
            index: Rc::new(RefCell::new(0)),
        }
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for tt in &self.tokens {
            write!(f, "{}", tt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
enum TokenTree {
    Group(Group),
    Token(Token),
}

impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenTree::Group(g) => write!(f, "{}", g.token_stream),
            TokenTree::Token(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Group {
    token_stream: TokenStream,
}

impl From<Vec<TokenTree>> for Group {
    fn from(tts: Vec<TokenTree>) -> Self {
        Self {
            token_stream: tts.into(),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Token {
    Ident(String),
    Literal(Literal),
    LeftParen,
    RightParen,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(s) => write!(f, "{}", s),
            Token::Literal(l) => write!(f, " {}", l),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
        }
    }
}

impl From<String> for TokenTree {
    fn from(s: String) -> Self {
        let t = match s.parse::<u32>() {
            Ok(u) => Token::Literal(Literal::UInt32(u)),
            Err(_) => Token::Ident(s),
        };
        TokenTree::Token(t)
    }
}

#[derive(Debug, PartialEq)]
enum Literal {
    UInt32(u32),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::UInt32(u) => write!(f, "{}", u),
        }
    }
}

fn main() -> Result<()> {
    let prog = String::from("(first (list 1 (+ 2 3) 9))");
    println!("prog: {}", prog);
    let chars = Rc::new(RefCell::new(prog.chars()));
    let input: TokenStream = chars.try_into()?;
    println!("tokenstream: {}", input);
    println!("tokenstream: {:?}", input);

    let ast: Expr = input.parse()?;
    println!("ast: {:?}", ast);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_given_example() {
        //let prog = String::from("(first (list 1 (+ 2 3) 9))");
        let chars = Rc::new(RefCell::new("(first (list 1 (+ 2 3) 9))".chars()));
        let input: Result<TokenStream> = chars.try_into();
        assert!(input.is_ok());
        let ts = input.unwrap();
        assert_eq!(
            ts,
            TokenStream::from(Vec::from([TokenTree::Group(Group::from(Vec::from([
                TokenTree::Token(Token::LeftParen),
                TokenTree::Token(Token::Ident(String::from("first"))),
                TokenTree::Group(Group::from(Vec::from([
                    TokenTree::Token(Token::LeftParen),
                    TokenTree::Token(Token::Ident(String::from("list"))),
                    TokenTree::Token(Token::Literal(Literal::UInt32(1))),
                    TokenTree::Group(Group::from(Vec::from([
                        TokenTree::Token(Token::LeftParen),
                        TokenTree::Token(Token::Ident(String::from("+"))),
                        TokenTree::Token(Token::Literal(Literal::UInt32(2))),
                        TokenTree::Token(Token::Literal(Literal::UInt32(3))),
                        TokenTree::Token(Token::RightParen),
                    ]))),
                    TokenTree::Token(Token::Literal(Literal::UInt32(9))),
                    TokenTree::Token(Token::RightParen),
                ]))),
                TokenTree::Token(Token::RightParen),
            ]))),]))
        );

        let ast: Result<Expr> = ts.parse();
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap(),
            Expr::List(Vec::from([Expr::Function(Function {
                name: Atom::Symbol("first".to_string()),
                args: Vec::from([Expr::Function(Function {
                    name: Atom::Symbol("list".to_string()),
                    args: Vec::from([
                        Expr::Atom(Atom::UInt32(1)),
                        Expr::Function(Function {
                            name: Atom::Symbol("+".to_string()),
                            args: Vec::from([
                                Expr::Atom(Atom::UInt32(2)),
                                Expr::Atom(Atom::UInt32(3)),
                            ]),
                        }),
                        Expr::Atom(Atom::UInt32(9)),
                    ])
                })])
            })]))
        );
    }

    #[test]
    fn test_simple_addition() -> Result<()> {
        let chars = Rc::new(RefCell::new("(+ 2 3)".chars()));
        let ts: TokenStream = chars.try_into()?;
        assert_eq!(
            ts,
            TokenStream::from(Vec::from([TokenTree::Group(Group::from(Vec::from([
                TokenTree::Token(Token::LeftParen),
                TokenTree::Token(Token::Ident(String::from("+"))),
                TokenTree::Token(Token::Literal(Literal::UInt32(2))),
                TokenTree::Token(Token::Literal(Literal::UInt32(3))),
                TokenTree::Token(Token::RightParen),
            ])))]))
        );

        let ast: Expr = ts.parse()?;
        assert_eq!(
            ast,
            Expr::List(Vec::from([Expr::Function(Function {
                name: Atom::Symbol("+".to_string()),
                args: Vec::from([Expr::Atom(Atom::UInt32(2)), Expr::Atom(Atom::UInt32(3)),]),
            })]))
        );
        Ok(())
    }
}
