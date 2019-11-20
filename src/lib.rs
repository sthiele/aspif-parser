use nom::{
    bytes::complete::{tag, take, take_while1},
    character::is_space,
    combinator::{map_res, opt},
    many1,
    multi::count,
    named, one_of, separated_list, tag, IResult,
};
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

pub fn read(file: &File) -> Result<(), std::io::Error> {
    let file = BufReader::new(file);
    // let mut graph = Graph::empty();
    for line in file.lines() {
        let l1 = line?;
        let l = l1.trim();
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
pub struct Statement();

#[derive(PartialEq, Clone, Debug)]
pub enum Head {
    Disjunction { elements: Vec<u64> },
    Choice { elements: Vec<u64> },
}
pub enum Body {
    NormalBody {
        elements: Vec<i64>,
    },
    WeightBody {
        lowerbound: u64,
        elements: Vec<WeightedBodyLit>,
    },
}
pub struct WeightedBodyLit {
    weight: u64,
    literal: i64,
}
#[derive(PartialEq, Clone, Debug)]
pub struct Rule {
    pub head: Head,
}
#[derive(PartialEq, Copy, Clone, Debug)]
enum Tags {
    Incremental,
}

fn pos_number(input: &str) -> IResult<&str, u64> {
    map_res(take_while1(is_dec_digit), from_dec)(input)
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

named!(zero_or_one<&str, char>, one_of!("01"));
named!(aspif_tags<&str, Vec<&str>>, separated_list!(tag(" "), string));

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

pub fn lrule(input: &str) -> IResult<&str, Rule> {
    let (input, head) = head(input)?;
    let (input, body) = body(input)?;
    Ok((input, Rule { head }))
}
pub fn head(input: &str) -> IResult<&str, Head> {
    let (input, bla) = zero_or_one(input)?;
    match bla {
        '0' => {
            let (input, _space) = tag(" ")(input)?;
            let (input, elements) = head_elements(input)?;
            Ok((input, Head::Disjunction { elements }))
        }
        '1' => {
            let (input, _space) = tag(" ")(input)?;
            let (input, elements) = head_elements(input)?;
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
            // let (input, elements) = body_elements(input)?;
            let elements = vec![];
            Ok((input, Body::NormalBody { elements }))
        }
        '1' => {
            let (input, _space) = tag(" ")(input)?;
            let lowerbound = 0;
            // let (input, elements) = weighted_body_elements(input)?;
            let elements = vec![];
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
fn head_elements(input: &str) -> IResult<&str, Vec<u64>> {
    let (input, size) = pos_number(input)?;
    let (input, elements) = count(pos_number_with_space, size as usize)(input)?;
    Ok((input, elements))
}

fn pos_number_with_space(input: &str) -> IResult<&str, u64> {
    let (input, _space) = tag(" ")(input)?;
    map_res(take_while1(is_dec_digit), from_dec)(input)
}
