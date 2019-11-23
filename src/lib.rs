use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while1},
    character::is_space,
    combinator::{map_res, opt},
    multi::count,
    named, not, one_of, separated_list, tag, IResult,
};
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

pub fn read(file: &File) -> Result<(), std::io::Error> {
    let file = BufReader::new(file);
    // let mut graph = Graph::empty();
    for line in file.lines() {
        let l1 = line?;
        // let l = l1.trim();
        // if !l.is_empty() {
        //     match aspif::statement(&l) {
        //         Ok(r) => {
        //             // graph.add(r);
        //         }
        //         Err(e) => println!("Parse error: {}", e),
        //     }
        // }
    }
    Ok(())
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Header {
    pub major: u64,
    pub minor: u64,
    pub revision: u64,
    pub incremental: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement<'a> {
    Rule(Rule),
    Minimize(Minimize),
    Projection(Vec<u64>),
    Output(Output<'a>),
    External{ atom: u64, init: Init},
    Assumption(Vec<i64>),
    Heuristic,
    Edge,
    Theory,
    Comment,
}
#[derive(PartialEq, Clone, Debug)]
pub enum Head {
    Disjunction { elements: Vec<u64> },
    Choice { elements: Vec<u64> },
}
#[derive(PartialEq, Clone, Debug)]
pub enum Body {
    NormalBody {
        elements: Vec<i64>,
    },
    WeightBody {
        lowerbound: u64,
        elements: Vec<(u64, i64)>,
    },
}
#[derive(PartialEq, Clone, Debug)]
pub struct Rule {
    pub head: Head,
    pub body: Body,
}
#[derive(PartialEq, Clone, Debug)]
pub struct Minimize {
    pub priority: u64,
    pub elements: Vec<(u64, i64)>,
}
#[derive(PartialEq, Clone, Debug)]
pub struct Output<'a> {
    pub string: &'a str,
    pub condition: Vec<i64>,
}
#[derive(PartialEq, Clone, Debug)]
pub enum Init {
    Free,
    True,
    False,
    Release
}
named!(not_zero<&str, ()>, not!(tag!("0")));
named!(zero_or_one<&str, char>, one_of!("01"));
named!(init_type<&str, char>, one_of!("0123"));
named!(heuristic_modifier<&str, char>, one_of!("012345"));
named!(statement_type<&str, char>, one_of!("123456789"));
named!(aspif_tags<&str, Vec<&str>>, separated_list!(tag(" "), string));

fn pos_number(input: &str) -> IResult<&str, u64> {
    map_res(take_while1(is_dec_digit), from_dec)(input)
}
fn pos_number2(input: &str) -> IResult<&str, i64> {
    let (input, num) = pos_number(input)?;
    Ok((input, num as i64))
}
fn neg_number(input: &str) -> IResult<&str, i64> {
    let (input, _bla) = tag("-")(input)?;
    let (input, num) = pos_number(input)?;
    Ok((input, -(num as i64)))
}
fn number(input: &str) -> IResult<&str, i64> {
    alt((pos_number2, neg_number))(input)
}
fn atom_id(input: &str) -> IResult<&str, u64> {
    let (input, _bla) = not_zero(input)?;
    map_res(take_while1(is_dec_digit), from_dec)(input)
}
fn negated_atom(input: &str) -> IResult<&str, i64> {
    let (input, _bla) = tag("-")(input)?;
    let (input, id) = atom_id(input)?;
    Ok((input, -(id as i64)))
}
fn pos_atom(input: &str) -> IResult<&str, i64> {
    let (input, id) = atom_id(input)?;
    Ok((input, id as i64))
}
fn literal(input: &str) -> IResult<&str, i64> {
    alt((pos_atom, negated_atom))(input)
}
fn weighted_literal(input: &str) -> IResult<&str, (u64, i64)> {
    let (input, weight) = pos_number(input)?;
    let (input, _space) = tag(" ")(input)?;
    let (input, literal) = literal(input)?;
    Ok((input, (weight, literal)))
}

fn from_dec(input: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(input, 10)
}

fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}

pub fn string(input: &str) -> IResult<&str, &str> {
    take_while1(is_alphanumeric)(input)
}
fn is_alphanumeric(c: char) -> bool {
    c.is_alphanumeric()
}

pub fn aspif_tags2(input: &str) -> IResult<&str, Vec<&str>> {
    let (input, _space) = tag(" ")(input)?;
    aspif_tags(input)
}

pub fn header(input: &str) -> IResult<&str, Header> {
    let (input, _tag) = tag("asp")(input)?;
    let (input, _space) = tag(" ")(input)?;
    let (input, major) = pos_number(input)?;
    let (input, _space) = tag(" ")(input)?;
    let (input, minor) = pos_number(input)?;
    let (input, _space) = tag(" ")(input)?;
    let (input, revision) = pos_number(input)?;
    let (input, optiional_tags) = opt(aspif_tags2)(input)?;
    let incremental = if let Some(tags) = optiional_tags {
        if let Some(_) = tags.iter().find(|x| **x == "incremental") {
            true
        } else {
            false
        }
    } else {
        false
    };
    Ok((
        input,
        Header {
            major,
            minor,
            revision,
            incremental,
        },
    ))
}

pub fn statement(input: &str) -> IResult<&str, Statement> {
    let (input, stype) = pos_number(input)?;
    match stype {
        1 => {
            let (input, _space) = tag(" ")(input)?;
            let (input, rule) = lrule(input)?;
            Ok((input, Statement::Rule(rule)))
        }
        2 => {
            let (input, _space) = tag(" ")(input)?;
            let (input, minimize) = minimize(input)?;
            Ok((input, Statement::Minimize(minimize)))
        }
        3 => {
            let (input, _space) = tag(" ")(input)?;
            let (input, atoms) = atoms(input)?;
            Ok((input, Statement::Projection(atoms)))
        }
        4 => {
            let (input, _space) = tag(" ")(input)?;
            let (input, output) = output(input)?;
            Ok((input, Statement::Output(output)))
        }
        5 => {
            let (input, _space) = tag(" ")(input)?;
            let (input, atom) = atom_id(input)?;
            
            let (input, init) = init_type(input)?;
            let init = match init {
                '0' => Init::Free,
                '1' => Init::True,
                '2' => Init::False,
                '3' => Init::Release,
                x => panic!("unmatched init type {}",x)
            };
            Ok((input, Statement::External{atom, init}))
        }
        6 => {
            let (input, _space) = tag(" ")(input)?;
            let (input, literals) = literals(input)?;
            Ok((input, Statement::Assumption(literals)))
        }
        7 => {
            let (input, _space) = tag(" ")(input)?;
            let (input, modifier) = heuristic_modifier(input)?;
            let (input, _space) = tag(" ")(input)?;
            let (input, k) = number(input)?;
            let (input, _space) = tag(" ")(input)?;
            let (input, prority) = pos_number(input)?;
            let (input, _space) = tag(" ")(input)?;
            let (input, literals) = literals(input)?;
            Ok((input, Statement::Heuristic))
        }
        8 => {
            let (input, _space) = tag(" ")(input)?;
            // let (input, rule) = lrule(input)?;
            Ok((input, Statement::Edge))
        }
        9 => {
            let (input, _space) = tag(" ")(input)?;
            // let (input, rule) = lrule(input)?;
            Ok((input, Statement::Theory))
        }
        10 => {
            let (input, _space) = tag(" ")(input)?;
            // let (input, rule) = lrule(input)?;
            Ok((input, Statement::Comment))
        }
        x => panic!("unmatched statement type {}", x),
    }
}
pub fn lrule(input: &str) -> IResult<&str, Rule> {
    let (input, head) = head(input)?;
    let (input, _space) = tag(" ")(input)?;
    let (input, body) = body(input)?;
    Ok((input, Rule { head, body }))
}
pub fn head(input: &str) -> IResult<&str, Head> {
    let (input, bla) = zero_or_one(input)?;
    match bla {
        '0' => {
            let (input, _space) = tag(" ")(input)?;
            let (input, elements) = atoms(input)?;
            Ok((input, Head::Disjunction { elements }))
        }
        '1' => {
            let (input, _space) = tag(" ")(input)?;
            let (input, elements) = atoms(input)?;
            Ok((input, Head::Choice { elements }))
        }
        _ => panic!("unmatched head type"),
    }
}
pub fn body(input: &str) -> IResult<&str, Body> {
    let (input, bla) = zero_or_one(input)?;
    match bla {
        '0' => {
            let (input, _space) = tag(" ")(input)?;
            let (input, elements) = literals(input)?;
            Ok((input, Body::NormalBody { elements }))
        }
        '1' => {
            let (input, _space) = tag(" ")(input)?;
            let (input, lowerbound) = pos_number(input)?;
            let (input, _space) = tag(" ")(input)?;
            let (input, elements) = weighted_literals(input)?;
            Ok((
                input,
                Body::WeightBody {
                    lowerbound,
                    elements,
                },
            ))
        }
        _ => panic!("unmatched head type"),
    }
}
fn atoms(input: &str) -> IResult<&str, Vec<u64>> {
    let (input, size) = pos_number(input)?;
    let (input, elements) = count(atom_id_with_space, size as usize)(input)?;
    Ok((input, elements))
}

fn atom_id_with_space(input: &str) -> IResult<&str, u64> {
    let (input, _space) = tag(" ")(input)?;
    atom_id(input)
}
fn literals(input: &str) -> IResult<&str, Vec<i64>> {
    let (input, size) = pos_number(input)?;
    let (input, elements) = count(literal_with_space, size as usize)(input)?;
    Ok((input, elements))
}

fn literal_with_space(input: &str) -> IResult<&str, i64> {
    let (input, _space) = tag(" ")(input)?;
    literal(input)
}
fn weighted_literals(input: &str) -> IResult<&str, Vec<(u64, i64)>> {
    let (input, size) = pos_number(input)?;
    let (input, elements) = count(weighted_literal_with_space, size as usize)(input)?;
    Ok((input, elements))
}

fn weighted_literal_with_space(input: &str) -> IResult<&str, (u64, i64)> {
    let (input, _space) = tag(" ")(input)?;
    weighted_literal(input)
}

pub fn minimize(input: &str) -> IResult<&str, Minimize> {
    let (input, priority) = pos_number(input)?;
    let (input, _space) = tag(" ")(input)?;
    let (input, elements) = weighted_literals(input)?;
    Ok((input, Minimize { priority, elements }))
}

pub fn output(input: &str) -> IResult<&str, Output> {
    let (input, len) = pos_number(input)?;
    let (input, _space) = tag(" ")(input)?;
    let (input, string) = take(len)(input)?;
    let (input, _space) = tag(" ")(input)?;
    let (input, condition) = literals(input)?;
    Ok((input, Output { string, condition }))
}
