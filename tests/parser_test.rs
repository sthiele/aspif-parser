use aspif_parser::*;

#[test]
fn string_test() {
    assert_eq!(string("incremental fu"), Ok((" fu", "incremental")));
}

#[test]
fn aspif_tags_test() {
    let res = vec!["incremental", "bla"];
    assert_eq!(aspif_tags2(" incremental bla"), Ok(("", res)));
}

#[test]
fn header_test() {
    assert_eq!(
        header("asp 12 0 223 incremental blub"),
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
        header("asp 12 0 223"),
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
    assert_eq!(head("1 3 22 3 1"), Ok(("", Head::Choice { elements: res })));
    let res = vec![22, 3, 1];
    assert_eq!(
        head("0 3 22 3 1"),
        Ok(("", Head::Disjunction { elements: res }))
    );
}
#[test]
fn body_test() {
    let res = vec![22, -3, 1];
    assert_eq!(
        body("0 3 22 -3 1"),
        Ok(("", Body::NormalBody { elements: res }))
    );
    let res = vec![(3, 22), (2, -3), (1, 1)];
    assert_eq!(
        body("1 55 3 3 22 2 -3 1 1"),
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
        lrule("1 3 22 3 1 1 55 3 3 22 2 -3 1 1"),
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
