use std::cell::RefCell;
use std::rc::Rc;
use std::str::Chars;

type Error = Box<dyn std::error::Error>;
type Result<T> = std::result::Result<T, Error>;

// Abstract Syntax Tree
#[derive(Debug)]
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
                TokenTree::Group(group) => match group.token_stream.peek(2) {
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

#[derive(Debug)]
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

#[derive(Debug)]
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
#[derive(Debug)]
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
                    tokens
                        .borrow_mut()
                        .push(TokenTree::Token(Token::RightParen));
                    terminate_acc();
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

#[derive(Debug)]
enum TokenTree {
    Group(Group),
    Token(Token),
}

#[derive(Debug)]
struct Group {
    token_stream: TokenStream,
}

#[derive(Debug)]
enum Token {
    Ident(String),
    Literal(Literal),
    LeftParen,
    RightParen,
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

#[derive(Debug)]
enum Literal {
    UInt32(u32),
}

fn main() -> Result<()> {
    let prog = String::from("(first (list 1 (+ 2 3) 9))");
    println!("prog: {}", prog);
    let chars = Rc::new(RefCell::new(prog.chars()));
    let input: TokenStream = chars.try_into()?;
    println!("tokenstream: {:?}", input);

    let ast: Expr = input.parse()?;
    println!("ast: {:?}", ast);
    Ok(())
}
