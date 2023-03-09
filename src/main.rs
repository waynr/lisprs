use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;

// Abstract Syntax Tree
struct Expr {
    ident: Atom,
    args: Vec<Atom>,
}

enum Atom {
    Symbol(String),
    Int(u32),
}

struct List {
    elements: Vec<Atom>,
}

// input handling
#[derive(Debug)]
struct TokenStream {
    tokens: Vec<Token>,
}

impl TryFrom<String> for TokenStream {
    type Error = Box<dyn Error>;

    fn try_from(s: String) -> Result<TokenStream, Box<dyn Error>> {
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

        for c in s.chars() {
            match c {
                '(' => {
                    terminate_acc();
                    tokens
                        .borrow_mut()
                        .push(Token::Separator(Separator::LeftParen));
                }
                ')' => {
                    terminate_acc();
                    tokens
                        .borrow_mut()
                        .push(Token::Separator(Separator::RightParen));
                }
                '+' => {
                    terminate_acc();
                    tokens.borrow_mut().push(Token::Operator(Operator::Plus));
                }
                ' ' => {
                    terminate_acc();
                }
                _ if c.is_alphanumeric() => sacc.clone().borrow_mut().push(c),
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
enum Token {
    Ident(String),
    Separator(Separator),
    Operator(Operator),
    Literal(Literal),
}

impl From<String> for Token {
    fn from(s: String) -> Token {
        match s.parse::<u32>() {
            Ok(u) => Token::Literal(Literal::UInt32(u)),
            Err(_) => Token::Ident(s),
        }
    }
}

#[derive(Debug)]
enum Separator {
    LeftParen,
    RightParen,
}

#[derive(Debug)]
enum Operator {
    Plus,
}

#[derive(Debug)]
enum Literal {
    UInt32(u32),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let prog = String::from("(first (list 1 (+ 2 3) 9))");
    let input: TokenStream = prog.try_into()?;
    println!("tokenstream: {:?}", input);
    Ok(())
}
