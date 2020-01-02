use aspif::*;
use nom::error::ErrorKind;

#[test]
fn string_test() {
    assert_eq!(
        string::<(&str, ErrorKind)>("incremental fu"),
        Ok((" fu", "incremental"))
    );
}

#[test]
fn aspif_tags_test() {
    let res = vec!["incremental", "bla"];
    assert_eq!(
        aspif_tags::<(&str, ErrorKind)>(" incremental bla"),
        Ok(("", res))
    );
}

#[test]
fn header_test() {
    assert_eq!(
        header::<(&str, ErrorKind)>("asp 12 0 223 incremental blub"),
        Ok((
            "",
            Header {
                major: 12,
                minor: 0,
                revision: 223,
                incremental: true
            }
        ))
    );
}
#[test]
fn header_test2() {
    assert_eq!(
        header::<(&str, ErrorKind)>("asp 12 0 223"),
        Ok((
            "",
            Header {
                major: 12,
                minor: 0,
                revision: 223,
                incremental: false
            }
        ))
    );
}

#[test]
fn head_test() {
    let res = vec![22, 3, 1];
    assert_eq!(
        head::<(&str, ErrorKind)>("1 3 22 3 1"),
        Ok(("", Head::Choice { elements: res }))
    );
    let res = vec![22, 3, 1];
    assert_eq!(
        head::<(&str, ErrorKind)>("0 3 22 3 1"),
        Ok(("", Head::Disjunction { elements: res }))
    );
}
#[test]
fn body_test() {
    let res = vec![22, -3, 1];
    assert_eq!(
        body::<(&str, ErrorKind)>("0 3 22 -3 1"),
        Ok(("", Body::NormalBody { elements: res }))
    );
    let res = vec![(3, 22), (2, -3), (1, 1)];
    assert_eq!(
        body::<(&str, ErrorKind)>("1 55 3 3 22 2 -3 1 1"),
        Ok((
            "",
            Body::WeightBody {
                lowerbound: 55,
                elements: res
            }
        ))
    );
}

#[test]
fn rule_test() {
    let res = vec![22, 3, 1];
    let res2 = vec![(3, 22), (2, -3), (1, 1)];
    assert_eq!(
        rule::<(&str, ErrorKind)>("1 3 22 3 1 1 55 3 3 22 2 -3 1 1"),
        Ok((
            "",
            Rule {
                head: Head::Choice { elements: res },
                body: Body::WeightBody {
                    lowerbound: 55,
                    elements: res2
                }
            }
        ))
    );
}
#[test]
fn statement_test() {
    let res = vec![22, 3, 1];
    let res2 = vec![(3, 22), (2, -3), (1, 1)];
    assert_eq!(
        statement::<(&str, ErrorKind)>("1 1 3 22 3 1 1 55 3 3 22 2 -3 1 1"),
        Ok((
            "",
            Statement::Rule(Rule {
                head: Head::Choice { elements: res },
                body: Body::WeightBody {
                    lowerbound: 55,
                    elements: res2
                }
            })
        ))
    );
    let res2 = vec![(3, 22), (2, -3), (1, 1)];
    assert_eq!(
        statement::<(&str, ErrorKind)>("2 55 3 3 22 2 -3 1 1"),
        Ok((
            "",
            Statement::Minimize(Minimize {
                priority: 55,
                elements: res2
            })
        ))
    );
    let res2 = vec![22, -3, 1];
    assert_eq!(
        statement::<(&str, ErrorKind)>("4 7 test2 x 3 22 -3 1"),
        Ok((
            "",
            Statement::Output(Output {
                string: "test2 x",
                condition: res2
            })
        ))
    );
}

#[test]
fn heuristic_test() {
    let res = vec![22, -3, 1];
    assert_eq!(
        statement::<(&str, ErrorKind)>("7 1 4 -5 3 3 22 -3 1"),
        Ok((
            "",
            Statement::Heuristic {
                modifier: HeuristicModifier::Sign,
                atom: 4,
                k: -5,
                priority: 3,
                condition: res,
            }
        ))
    );
}
#[test]
fn edge_test() {
    let res = vec![22, -3, 1];
    assert_eq!(
        statement::<(&str, ErrorKind)>("8 -1 4 3 22 -3 1"),
        Ok((
            "",
            Statement::Edge {
                u: -1,
                v: 4,
                condition: res,
            }
        ))
    );
}
