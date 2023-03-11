use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;
use std::str::Chars;

// Abstract Syntax Tree
enum Expr {
    Function(Function),
    Atom(Atom),
}

struct Function {
    ident: Atom,
    args: Vec<Expr>,
}

enum Atom {
    Symbol(String),
    Int(u32),
}

struct List {
    elements: Vec<Atom>,
}

trait Parse: Sized {
    fn parse(ts: &mut TokenStream) -> Result<Self, Box<dyn Error>>;
}

// input handling
#[derive(Debug)]
struct TokenStream {
    tokens: Vec<TokenTree>,
}

impl TryFrom<Rc<RefCell<Chars<'_>>>> for TokenStream {
    type Error = Box<dyn Error>;

    fn try_from(cs: Rc<RefCell<Chars>>) -> Result<Self, Box<dyn Error>> {
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
                                    return Err(Box::<dyn Error>::from(
                                        format!("missing final parenthesis!").to_string(),
                                    ));
                                }
                            },
                            _ => {
                                return Err(Box::<dyn Error>::from(
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
                    return Err(Box::<dyn Error>::from(
                        format!("unsupported character {}", c).to_string(),
                    ));
                }
            }
        }
        Ok(Self {
            tokens: tokens.take(),
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

impl TokenStream {
    fn parse<T: Parse>(&mut self) -> Result<T, Box<dyn Error>> {
        T::parse(self)
    }
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
enum DelimiterType {
    Parenthesis,
}

#[derive(Debug)]
enum Literal {
    UInt32(u32),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let prog = String::from("(first (list 1 (+ 2 3) 9))");
    let chars = Rc::new(RefCell::new(prog.chars()));
    let input: TokenStream = chars.try_into()?;
    println!("tokenstream: {:?}", input);
    Ok(())
}
