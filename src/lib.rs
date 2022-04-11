use nom::error::{convert_error, FromExternalError, ParseError, VerboseError};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while1},
    character::complete::{char, one_of},
    combinator::{map_res, not, opt},
    multi::{count, many_till, separated_list1},
    Err, IResult,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum AspifError {
    #[error("ParseError: {msg}")]
    ParseError { msg: String },
}

pub fn read_aspif<'a>(input: &'a str) -> Result<AspifProgram, AspifError> {
    match aspif_program::<VerboseError<&str>>(input) {
        Ok((_, result)) => Ok(result),

        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            let msg = convert_error(input, e);
            Err(AspifError::ParseError { msg })
        }
        Err(e) => Err(AspifError::ParseError {
            msg: format!("Failed to parse aspif!\n{e}"),
        }
        .into()),
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct AspifProgram<'a> {
    pub header: Header,
    pub statements: Vec<Statement<'a>>,
}
pub fn aspif_program<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, AspifProgram, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, header) = header(input)?;
    let (input, _nl) = char('\n')(input)?;
    let (input, (statements, _)) = many_till(nl_statement, aspif_end)(input)?;
    Ok((input, AspifProgram { header, statements }))
}
#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Header {
    pub major: u64,
    pub minor: u64,
    pub revision: u64,
    pub incremental: bool,
}
pub fn header<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Header, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _tag) = tag("asp")(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, major) = pos_number_as_u64(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, minor) = pos_number_as_u64(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, revision) = pos_number_as_u64(input)?;
    let (input, optional_tags) = opt(aspif_tags)(input)?;
    let incremental = if let Some(tags) = optional_tags {
        tags.iter().any(|x| *x == "incremental")
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
#[derive(PartialEq, Clone, Debug)]
pub enum Statement<'a> {
    Rule(Rule),
    Minimize(Minimize),
    Projection(Vec<u64>),
    Output(Output<'a>),
    External {
        atom: u64,
        init: Init,
    },
    Assumption(Vec<i64>),
    Heuristic {
        modifier: HeuristicModifier,
        atom: u64,
        k: i64,
        priority: u64,
        condition: Vec<i64>,
    },
    Edge {
        u: i64,
        v: i64,
        condition: Vec<i64>,
    },
    NumericTheoryTerm {
        id: u64,
        w: i64,
    },
    SymbolicTheoryTerm {
        id: u64,
        string: &'a str,
    },
    CompoundTheoryTerm {
        id: u64,
        t: TheoryTermType,
        terms: Vec<u64>,
    },
    TheoryAtomElements {
        id: u64,
        theory_terms: Vec<u64>,
        literals: Vec<i64>,
    },
    TheoryAtom {
        atom: u64,
        symbolic_term: u64,
        theory_atom_elements: Vec<u64>,
        theory_operation: Option<(u64, u64)>,
    },
    TheoryDirective {
        symbolic_term: u64,
        theory_atom_elements: Vec<u64>,
        theory_operation: Option<(u64, u64)>,
    },
    Comment,
}

pub fn statement<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, stype) = statement_type(input)?;
    match stype {
        StatementType::Rule => {
            let (input, _space) = char(' ')(input)?;
            let (input, rule) = rule(input)?;
            Ok((input, Statement::Rule(rule)))
        }
        StatementType::Minimize => {
            let (input, _space) = char(' ')(input)?;
            let (input, minimize) = minimize(input)?;
            Ok((input, Statement::Minimize(minimize)))
        }
        StatementType::Projection => {
            let (input, _space) = char(' ')(input)?;
            let (input, atoms) = atoms(input)?;
            Ok((input, Statement::Projection(atoms)))
        }
        StatementType::Output => {
            let (input, _space) = char(' ')(input)?;
            let (input, output) = output(input)?;
            Ok((input, Statement::Output(output)))
        }
        StatementType::External => {
            let (input, _space) = char(' ')(input)?;
            let (input, atom) = non_zero_pos_number_as_u64(input)?;
            let (input, init) = init_type(input)?;
            Ok((input, Statement::External { atom, init }))
        }
        StatementType::Assumption => {
            let (input, _space) = char(' ')(input)?;
            let (input, literals) = literals(input)?;
            Ok((input, Statement::Assumption(literals)))
        }
        StatementType::Heuristic => {
            let (input, _space) = char(' ')(input)?;
            let (input, modifier) = heuristic_modifier(input)?;
            let (input, _space) = char(' ')(input)?;
            let (input, atom) = non_zero_pos_number_as_u64(input)?;
            let (input, _space) = char(' ')(input)?;
            let (input, k) = number(input)?;
            let (input, _space) = char(' ')(input)?;
            let (input, priority) = pos_number_as_u64(input)?;
            let (input, _space) = char(' ')(input)?;
            let (input, condition) = literals(input)?;
            Ok((
                input,
                Statement::Heuristic {
                    modifier,
                    atom,
                    k,
                    priority,
                    condition,
                },
            ))
        }
        StatementType::Edge => {
            let (input, _space) = char(' ')(input)?;
            let (input, u) = number(input)?;
            let (input, _space) = char(' ')(input)?;
            let (input, v) = number(input)?;
            let (input, _space) = char(' ')(input)?;
            let (input, condition) = literals(input)?;
            Ok((input, Statement::Edge { u, v, condition }))
        }
        StatementType::TheoryStatement => {
            let (input, _space) = char(' ')(input)?;
            let (input, theory_statement_type) = one_of("012456")(input)?;
            // let (input, theory_statement_type) = match theory_statement_type {
            //     Err(Err::Error(e)) => {
            //         return Err(Err::Error(nom::error::ParseError::add_context(
            //             input,
            //             "theory statement type, expected one of 0,1,2,4,5,6",
            //             e,
            //         )));
            //     }
            //     Err(Err::Failure(e)) => {
            //         return Err(Err::Failure(nom::error::ParseError::add_context(
            //             input,
            //             "theory statement type, expected one of 0,1,2,4,5,6",
            //             e,
            //         )))
            //     }
            //     Err(Err::Incomplete(e)) => return Err(Err::Incomplete(e)),
            //     Ok(x) => x,
            // };
            let (input, _space) = char(' ')(input)?;
            match theory_statement_type {
                '0' => {
                    let (input, id) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, w) = number(input)?;
                    Ok((input, Statement::NumericTheoryTerm { id, w }))
                }
                '1' => {
                    let (input, id) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, len) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, string) = take(len)(input)?;
                    Ok((input, Statement::SymbolicTheoryTerm { id, string }))
                }
                '2' => {
                    let (input, id) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, t) = theory_term_type(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, terms) = theory_terms(input)?;
                    Ok((input, Statement::CompoundTheoryTerm { id, t, terms }))
                }
                '4' => {
                    let (input, id) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, theory_terms) = theory_terms(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, literals) = literals(input)?;
                    Ok((
                        input,
                        Statement::TheoryAtomElements {
                            id,
                            theory_terms,
                            literals,
                        },
                    ))
                }
                '5' => {
                    let (input, atom) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, symbolic_term) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, theory_atom_elements) = theory_terms(input)?;
                    if atom > 0 {
                        Ok((
                            input,
                            Statement::TheoryAtom {
                                atom,
                                symbolic_term,
                                theory_atom_elements,
                                theory_operation: None,
                            },
                        ))
                    } else {
                        Ok((
                            input,
                            Statement::TheoryDirective {
                                symbolic_term,
                                theory_atom_elements,
                                theory_operation: None,
                            },
                        ))
                    }
                }
                '6' => {
                    let (input, atom) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, symbolic_term) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, theory_atom_elements) = theory_terms(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, theory_operator) = pos_number_as_u64(input)?;
                    let (input, _space) = char(' ')(input)?;
                    let (input, theory_term) = pos_number_as_u64(input)?;

                    if atom > 0 {
                        Ok((
                            input,
                            Statement::TheoryAtom {
                                atom,
                                symbolic_term,
                                theory_atom_elements,
                                theory_operation: Some((theory_operator, theory_term)),
                            },
                        ))
                    } else {
                        Ok((
                            input,
                            Statement::TheoryDirective {
                                symbolic_term,
                                theory_atom_elements,
                                theory_operation: Some((theory_operator, theory_term)),
                            },
                        ))
                    }
                }
                x => panic!("unmatched theory term type type {}", x),
            }
        }
        StatementType::Comment => {
            let (input, _space) = char(' ')(input)?;
            Ok((input, Statement::Comment))
        }
    }
}
pub fn aspif_end<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, char, E> {
    char('0')(input)
}
#[derive(PartialEq, Clone, Debug)]
pub struct Rule {
    pub head: Head,
    pub body: Body,
}
pub fn rule<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Rule, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, head) = head(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, body) = body(input)?;
    Ok((input, Rule { head, body }))
}
#[derive(PartialEq, Clone, Debug)]
pub enum Head {
    Disjunction { elements: Vec<u64> },
    Choice { elements: Vec<u64> },
}
pub fn head<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Head, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, bla) = one_of("01")(input)?;
    match bla {
        '0' => {
            let (input, _space) = char(' ')(input)?;
            let (input, elements) = atoms(input)?;
            Ok((input, Head::Disjunction { elements }))
        }
        '1' => {
            let (input, _space) = char(' ')(input)?;
            let (input, elements) = atoms(input)?;
            Ok((input, Head::Choice { elements }))
        }
        x => panic!("unmatched head type {}", x),
    }
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
pub fn body<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Body, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, bla) = one_of("01")(input)?;
    match bla {
        '0' => {
            let (input, _space) = char(' ')(input)?;
            let (input, elements) = literals(input)?;
            Ok((input, Body::NormalBody { elements }))
        }
        '1' => {
            let (input, _space) = char(' ')(input)?;
            let (input, lowerbound) = pos_number_as_u64(input)?;
            let (input, _space) = char(' ')(input)?;
            let (input, elements) = weighted_literals(input)?;
            Ok((
                input,
                Body::WeightBody {
                    lowerbound,
                    elements,
                },
            ))
        }
        x => panic!("unmatched head type {}", x),
    }
}
#[derive(PartialEq, Clone, Debug)]
pub struct Minimize {
    pub priority: u64,
    pub elements: Vec<(u64, i64)>,
}
pub fn minimize<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Minimize, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, priority) = pos_number_as_u64(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, elements) = weighted_literals(input)?;
    Ok((input, Minimize { priority, elements }))
}
#[derive(PartialEq, Clone, Debug)]
pub struct Output<'a> {
    pub string: &'a str,
    pub condition: Vec<i64>,
}
pub fn output<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Output, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, len) = pos_number_as_u64(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, string) = take(len)(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, condition) = literals(input)?;
    Ok((input, Output { string, condition }))
}
#[derive(PartialEq, Clone, Debug)]
pub enum Init {
    Free,
    True,
    False,
    Release,
}
pub fn init_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Init, E> {
    let (input, init) = one_of("0123")(input)?;
    match init {
        '0' => Ok((input, Init::Free)),
        '1' => Ok((input, Init::True)),
        '2' => Ok((input, Init::False)),
        '3' => Ok((input, Init::Release)),
        x => panic!("unmatched init type {}", x),
    }
}
#[derive(PartialEq, Clone, Debug)]
pub enum HeuristicModifier {
    Level,
    Sign,
    Factor,
    Init,
    True,
    False,
}
fn heuristic_modifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, HeuristicModifier, E> {
    let (input, modifier) = one_of("012345")(input)?;
    match modifier {
        '0' => Ok((input, HeuristicModifier::Level)),
        '1' => Ok((input, HeuristicModifier::Sign)),
        '2' => Ok((input, HeuristicModifier::Factor)),
        '3' => Ok((input, HeuristicModifier::Init)),
        '4' => Ok((input, HeuristicModifier::True)),
        '5' => Ok((input, HeuristicModifier::False)),
        x => panic!("unmatched heuristic modifier type {}", x),
    }
}
#[derive(PartialEq, Clone, Debug)]
pub enum TheoryTermType {
    Tuple,
    Braces,
    Brackets,
    SymbolicTermId(u64),
}
enum StatementType {
    Rule,
    Minimize,
    Projection,
    Output,
    External,
    Assumption,
    Heuristic,
    Edge,
    TheoryStatement,
    Comment,
}
fn statement_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, StatementType, E> {
    let (input, digit) = one_of("123456789")(input)?;
    match digit {
        '1' => {
            let (input, ten) = opt(char('0'))(input)?;
            if let Some('0') = ten {
                Ok((input, StatementType::Comment))
            } else {
                Ok((input, StatementType::Rule))
            }
        }
        '2' => Ok((input, StatementType::Minimize)),
        '3' => Ok((input, StatementType::Projection)),
        '4' => Ok((input, StatementType::Output)),
        '5' => Ok((input, StatementType::External)),
        '6' => Ok((input, StatementType::Assumption)),
        '7' => Ok((input, StatementType::Heuristic)),
        '8' => Ok((input, StatementType::Edge)),
        '9' => Ok((input, StatementType::TheoryStatement)),
        x => panic!("unmatched statement type {}", x),
    }
}
fn nl_statement<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, statement) = statement(input)?;
    let (input, _) = char('\n')(input)?;
    Ok((input, statement))
}
// fn pos_number_as_u64<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, u64, E> {
//     map_res(take_while1(is_dec_digit), from_dec)(input)
// }

fn pos_number_as_u64<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, u64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, int) = map_res(take_while1(is_dec_digit), from_dec)(input)?;
    Ok((input, int as u64))
}

fn pos_number_as_i64<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, num) = pos_number_as_u64(input)?;
    Ok((input, num as i64))
}
fn neg_number<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _bla) = char('-')(input)?;
    let (input, num) = pos_number_as_u64(input)?;
    Ok((input, -(num as i64)))
}
fn number<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    alt((pos_number_as_i64, neg_number))(input)
}
fn non_zero_pos_number_as_u64<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, u64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _bla) = not(char('0'))(input)?;
    let (input, num) = pos_number_as_u64(input)?;
    Ok((input, num))
}
fn non_zero_pos_number_as_i64<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, i64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _bla) = not(char('0'))(input)?;
    let (input, num) = pos_number_as_i64(input)?;
    Ok((input, num))
}
fn negated_atom<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _bla) = char('-')(input)?;
    let (input, id) = non_zero_pos_number_as_u64(input)?;
    Ok((input, -(id as i64)))
}
fn pos_atom<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, id) = non_zero_pos_number_as_u64(input)?;
    Ok((input, id as i64))
}
fn literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    alt((pos_atom, negated_atom))(input)
}
fn weighted_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (u64, i64), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, weight) = pos_number_as_u64(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, literal) = literal(input)?;
    Ok((input, (weight, literal)))
}
fn theory_term_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, TheoryTermType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, t) = alt((special, non_zero_pos_number_as_i64))(input)?;
    match t {
        -1 => Ok((input, TheoryTermType::Tuple)),
        -2 => Ok((input, TheoryTermType::Braces)),
        -3 => Ok((input, TheoryTermType::Brackets)),
        x if x > 0 => Ok((input, TheoryTermType::SymbolicTermId(x as u64))),
        x => panic!("unmatched theory term type {}", x),
    }
}
fn special<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E> {
    let (input, _bla) = char('-')(input)?;
    let (input, num) = one_of("123")(input)?;
    match num {
        '1' => Ok((input, -1)),
        '2' => Ok((input, -2)),
        '3' => Ok((input, -3)),
        x => panic!("unmatched special theory term type {}", x),
    }
}
fn from_dec(input: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(input, 10)
}
fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}
pub fn string<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while1(is_alphanumeric)(input)
}
fn is_alphanumeric(c: char) -> bool {
    c.is_alphanumeric()
}
pub fn aspif_tags<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<&'a str>, E> {
    let (input, _space) = char(' ')(input)?;
    separated_list1(char(' '), string)(input)
}
fn atoms<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<u64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, size) = pos_number_as_u64(input)?;
    let (input, elements) = count(atom_id_with_space, size as usize)(input)?;
    Ok((input, elements))
}
fn atom_id_with_space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, u64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _space) = char(' ')(input)?;
    non_zero_pos_number_as_u64(input)
}
fn literals<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<i64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, size) = pos_number_as_u64(input)?;
    let (input, elements) = count(literal_with_space, size as usize)(input)?;
    Ok((input, elements))
}
fn literal_with_space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _space) = char(' ')(input)?;
    literal(input)
}
fn weighted_literals<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<(u64, i64)>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, size) = pos_number_as_u64(input)?;
    let (input, elements) = count(weighted_literal_with_space, size as usize)(input)?;
    Ok((input, elements))
}
fn weighted_literal_with_space<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (u64, i64), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _space) = char(' ')(input)?;
    weighted_literal(input)
}
fn theory_terms<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<u64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, size) = pos_number_as_u64(input)?;
    let (input, elements) = count(theory_term_id_with_space, size as usize)(input)?;
    Ok((input, elements))
}
fn theory_term_id_with_space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, u64, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _space) = char(' ')(input)?;
    pos_number_as_u64(input)
}
