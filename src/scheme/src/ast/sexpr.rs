use self::SExpr::*;

use std::borrow::Borrow;

pub type ParseResult<A> = Result<A, Error>;

#[derive(Debug)]
pub enum Error {
    GotEOF(usize, String),
    NothingParsed,
    NotASingleton(char),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum SExpr {
    Bool(bool),
    Sym(String),
    Int(i64),
    List(Vec<SExpr>),
}

impl SExpr {
    pub fn is_list(&self) -> bool {
        match self {
            &List(..) => true,
            _ => false,
        }
    }

    pub fn unwrap_list(&self) -> Result<&[SExpr], String> {
        match self {
            &List(ref es) => Ok(es),
            _ => Err(format!("Not a list: {:?}", self)),
        }
    }

    pub fn unwrap_sym(&self) -> Result<&str, String> {
        match self {
            &Sym(ref s) => Ok(s),
            _ => Err(format!("Not a sym: {:?}", self)),
        }
    }
}

pub fn parse_many(xs: &str) -> ParseResult<Vec<SExpr>> {
    let xs: Vec<char> = xs.chars().collect();
    let mut pos = 0;
    let mut res: Vec<SExpr> = vec![];
    while pos < xs.len() {
        let got = parse_at(&xs, &mut pos);
        match got {
            Err(Error::NothingParsed) => {
                continue;
            }
            Err(e) => {
                return Err(e);
            }
            Ok(got) => {
                res.push(got);
            }
        }
    }
    Ok(res)
}

fn parse_at(xs: &[char], i: &mut usize) -> ParseResult<SExpr> {
    while *i < xs.len() {
        let x = xs[*i];
        *i += 1;
        match x {
            '(' => return parse_list_at(xs, i),
            '\'' => return Ok(quoted("quote", parse_at(xs, i)?)),
            ';' => {
                skip_this_line(xs, i);
                continue;
            }
            '#' => {
                return parse_singleton_at(xs, i);
            }
            _ if x.is_whitespace() => continue,
            _ if x.is_numeric() => {
                *i -= 1;
                return parse_int_at(xs, i);
            }
            _ => {
                *i -= 1;
                return parse_sym_at(xs, i);
            }
        }
    }
    Err(Error::NothingParsed)
}

fn skip_this_line(xs: &[char], i: &mut usize) {
    while *i < xs.len() {
        let x = xs[*i];
        *i += 1;
        if x == '\n' {
            break;
        }
    }
}

fn quoted(tag: &str, e: SExpr) -> SExpr {
    list(&[sym(tag), e])
}

fn parse_int_at(xs: &[char], i: &mut usize) -> ParseResult<SExpr> {
    let mut res = 0_i64;
    while *i < xs.len() {
        let x = xs[*i];
        *i += 1;
        match x {
            _ if x.is_numeric() => {
                res *= 10;
                res += x.to_digit(10).unwrap() as i64;
            }
            _ => {
                *i -= 1;
                return Ok(Int(res));
            }
        }
    }
    Ok(Int(res))
}

fn parse_sym_at(xs: &[char], i: &mut usize) -> ParseResult<SExpr> {
    let mut res = vec![];
    while *i < xs.len() {
        let x = xs[*i];
        *i += 1;
        match x {
            _ if x.is_whitespace() || x == '(' || x == ')' => {
                *i -= 1;
                return Ok(Sym(res.into_iter().collect()));
            }
            _ => {
                res.push(x);
            }
        }
    }
    Ok(Sym(res.into_iter().collect()))
}

fn parse_list_at(xs: &[char], i: &mut usize) -> ParseResult<SExpr> {
    let mut res = vec![];
    while *i < xs.len() {
        let x = xs[*i];
        *i += 1;
        match x {
            ')' => return Ok(List(res)),
            _ if x.is_whitespace() => continue,
            _ => {
                *i -= 1;
                res.push(parse_at(xs, i)?);
            }
        }
    }
    Err(Error::GotEOF(*i, "parsing list, expecting `)`".to_owned()))
}

fn parse_singleton_at(xs: &[char], i: &mut usize) -> ParseResult<SExpr> {
    let x = xs[*i];
    *i += 1;

    Ok(match x {
        't' => Bool(true),
        'f' => Bool(false),
        _ => return Err(Error::NotASingleton(x)),
    })
}

pub fn list<A>(xs: &[A]) -> SExpr
    where A: Borrow<SExpr>
{
    List(xs.iter().map(|x| x.borrow().clone()).collect())
}

pub fn int(i: i64) -> SExpr {
    Int(i)
}

pub fn sym(s: &str) -> SExpr {
    Sym(s.to_owned())
}

#[test]
fn test_parse() {
    fn parse_one(s: &str) -> SExpr {
        let got = parse_many(s).unwrap();
        assert!(got.len() == 1);
        got.into_iter().next().unwrap()
    }

    let nil = list::<SExpr>(&[]);
    assert_eq!(parse_one("(let ((a 5)) a)"),
               list(&[&sym("let"), &list(&[&list(&[sym("a"), int(5)])]), &sym("a")]));

    assert_eq!(parse_one("()"), nil.clone());
    assert_eq!(parse_one("(1)"), list(&[int(1)]));
    assert_eq!(parse_one("(+ 1 2)"), list(&[sym("+"), int(1), int(2)]));
    assert_eq!(parse_one("(+ (+ 1 2) 3)"),
               list(&[sym("+"), list(&[sym("+"), int(1), int(2)]), int(3)]));
    assert_eq!(parse_one(" () "), nil);
    assert_eq!(parse_one("(())"), list(&[&nil]));
    assert_eq!(parse_many(" () () 5 + 7").unwrap(),
               vec![nil.clone(), nil.clone(), int(5), sym("+"), int(7)]);
}
