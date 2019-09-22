#[macro_use]
extern crate nom;
// TODO: need to change this to a line-based parse to handle comments etc.
use nom::{
    branch::alt,
    bytes::complete::{is_a, tag, take_while, take_while_m_n},
    character::complete::{alphanumeric1, char, digit1, multispace0, none_of, one_of, space0},
    character::is_digit,
    combinator::{map, not, opt, peek},
    multi::{count, many0, many1, separated_list, many_till},
    number::complete::double,
    sequence::{preceded, terminated},
    IResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct NamelistFile {
    pub namelists: Vec<Namelist>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Namelist {
    pub name: String,
    pub parameters: Vec<Parameter>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub value: ParameterValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterValue {
    Atom(ParameterValueAtom),
    Array(ParamPos, Vec<ParameterValueAtom>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterValueAtom {
    String(String),
    Double(f64),
    Int(i64),
    Bool(bool),
}

impl From<String> for ParameterValue {
    fn from(val: String) -> Self {
        ParameterValue::Atom(val.into())
    }
}

impl From<f64> for ParameterValue {
    fn from(val: f64) -> Self {
        ParameterValue::Atom(val.into())
    }
}

impl From<i64> for ParameterValue {
    fn from(val: i64) -> Self {
        ParameterValue::Atom(val.into())
    }
}

impl From<bool> for ParameterValue {
    fn from(val: bool) -> Self {
        ParameterValue::Atom(val.into())
    }
}

impl From<String> for ParameterValueAtom {
    fn from(val: String) -> Self {
        ParameterValueAtom::String(val)
    }
}

impl From<f64> for ParameterValueAtom {
    fn from(val: f64) -> Self {
        ParameterValueAtom::Double(val)
    }
}

impl From<i64> for ParameterValueAtom {
    fn from(val: i64) -> Self {
        ParameterValueAtom::Int(val)
    }
}

impl From<bool> for ParameterValueAtom {
    fn from(val: bool) -> Self {
        ParameterValueAtom::Bool(val)
    }
}

/// A boolean is either an F or T (case insensitive) followed by any series of
/// non-whitespace characters. It may also pre prepended by a period.
pub fn boolean(i: &[u8]) -> IResult<&[u8], bool> {
    let (i, _) = opt(char('.'))(i)?;
    let (i, cs) = is_a("tTfF")(i)?;
    let (i, _) = many0(none_of(" \t\r\n/,"))(i)?;
    match cs {
        b"t" | b"T" => Ok((i, true)),
        b"f" | b"F" => Ok((i, false)),
        _ => unreachable!(),
    }
}

pub fn parse_int(i: &[u8]) -> IResult<&[u8], i64> {
    let (i, s) = opt(alt((char('+'), char('-'))))(i)?;
    let (i, digits) = digit1(i)?;
    // allow a trailing decimal point and trailing zeros
    let (i, period) = opt(char('.'))(i)?;
    let i = if period.is_some() {
        many0(char('0'))(i)?.0
    } else {
        i
    };
    // TODO: fix this error handling
    let num = std::str::from_utf8(digits)
        .expect("not valid utf8")
        .parse::<i64>()
        .expect("not a valid number");
    if s == Some('-') {
        Ok((i, -1 * num))
    } else {
        Ok((i, num))
    }
}

pub fn parse_uint(i: &[u8]) -> IResult<&[u8], u64> {
    let (i, digits) = digit1(i)?;
    // TODO: fix this error handling
    let num = std::str::from_utf8(digits)
        .expect("not valid utf8")
        .parse::<u64>()
        .expect("not a valid number");
    Ok((i, num))
}

pub fn parse_double(i: &[u8]) -> IResult<&[u8], f64> {
    // must start with either a digit or a decimal point.
    peek(one_of("1234567890+-."))(i)?;
    double(i)
}

pub fn parameter_name(i: &[u8]) -> IResult<&[u8], String> {
    let (i, _) = peek(none_of("&=/( \r\n\t0123456789.,"))(i)?;
    let (i, p_name) = many1(none_of("=/( \r\n\t.,"))(i)?;
    Ok((i, p_name.into_iter().collect()))
}

pub fn parse_nml_name(i: &[u8]) -> IResult<&[u8], String> {
    let (i, name) = alphanumeric1(i)?;
    Ok((
        i,
        std::str::from_utf8(name)
            .expect("namelist name is not valid utf8")
            .to_string(),
    ))
}

pub fn quoted_string(i: &[u8]) -> IResult<&[u8], String> {
    alt((quoted_string_single, quoted_string_double))(i)
}


pub fn quoted_string_single(i: &[u8]) -> IResult<&[u8], String> {
    let (i, _start_quote) = char('\'')(i)?;
    let (i, s) = many0(none_of("\'"))(i)?;
    let (i, _end_quote) = char('\'')(i)?;
    Ok((i, s.iter().collect()))
}


pub fn quoted_string_double(i: &[u8]) -> IResult<&[u8], String> {
    let (i, _start_quote) = char('\"')(i)?;
    let (i, s) = many0(none_of("\""))(i)?;
    let (i, _end_quote) = char('\"')(i)?;
    Ok((i, s.iter().collect()))
}

// TODO: make sure this is case insensitive
pub type GroupSpec = std::collections::HashMap<String, ParameterSpec>;
pub type NamelistSpec = std::collections::HashMap<String, GroupSpec>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ParameterSpec {
    Atom(ParameterSpecAtom),
    Array(ParameterSpecAtom),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ParameterSpecAtom {
    String,
    Double,
    Int,
    Bool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ParamPos {
    OneDim(Range),
    TwoDim(Range, Range),
}

impl ParamPos {
    pub fn len(&self) -> usize {
        use ParamPos::*;
        match self {
            OneDim(range) => range.len(),
            TwoDim(r1, Range::SingleNumber(_)) => r1.len(),
            TwoDim(Range::SingleNumber(_), r2) => r2.len(),
            TwoDim(_, _) => panic!(
                "In a two-dimensionsal position parameter, one dimensions must be a single number"
            ),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Range {
    /// i.e. ":"
    Numberless,
    SingleNumber(u64),
    TwoNumber(u64, u64),
}

impl Range {
    pub fn len(&self) -> usize {
        match self {
            Range::Numberless => panic!("not enough information"),
            Range::SingleNumber(_) => 1_usize,
            Range::TwoNumber(m, n) => (n - m + 1) as usize,
        }
    }
}

// TODO: Technically arrays can have more dimensions.
// Can be either one or two range values
pub fn param_pos(i: &[u8]) -> IResult<&[u8], ParamPos> {
    let (i, _start_paren) = char('(')(i)?;
    let (i, r1) = parse_range(i)?;
    let (i, r2_opt) = opt(preceded(parse_comma_sep, parse_range))(i)?;
    let (i, _end_paren) = char(')')(i)?;
    let pos = match r2_opt {
        Some(r2) => ParamPos::TwoDim(r1, r2),
        None => ParamPos::OneDim(r1),
    };
    Ok((i, pos))
}

/// Parses values in the form of "x" or "x:y" where x and y are integers, e.g.
/// "1:2", or possibly just ":".
pub fn parse_range(i: &[u8]) -> IResult<&[u8], Range> {
    alt((
        parse_twonumber_range,
        parse_singlenumber_range,
        parse_numberless_range,
    ))(i)
}

/// i.e. ":"
fn parse_numberless_range(i: &[u8]) -> IResult<&[u8], Range> {
    let (i, _) = char(':')(i)?;
    Ok((i, Range::Numberless))
}

/// i.e. "5"
fn parse_singlenumber_range(i: &[u8]) -> IResult<&[u8], Range> {
    // TODO: needs to be a natural greater than 0
    let (i, s1) = parse_uint(i)?;
    Ok((i, Range::SingleNumber(s1)))
}

/// i.e. "3:5"
fn parse_twonumber_range(i: &[u8]) -> IResult<&[u8], Range> {
    // TODO: needs to be a natural greater than 0
    let (i, _) = multispace0(i)?;
    let (i, s1) = parse_uint(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = char(':')(i)?;
    let (i, _) = multispace0(i)?;
    let (i, s2) = parse_uint(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, Range::TwoNumber(s1, s2)))
}

/// Parses a comma, possibly surrounded by spaces, or possibly EOF
pub fn parse_comma_sep(i: &[u8]) -> IResult<&[u8], ()> {
    let (i, _) = many1(parse_comma_sep_single)(i)?;
    Ok((i, ()))
}

pub fn parse_comma_sep_single(i: &[u8]) -> IResult<&[u8], ()> {
    let (i, _) = peek(one_of(" \r\n\t,"))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = opt(char(','))(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, ()))
}

pub fn parse_namelist_file<'a, 'b>(
    namelist_spec: &'a NamelistSpec,
    i: &'b [u8],
) -> IResult<&'b [u8], NamelistFile> {
    // Skip to the first namelise
    let (i, _) = many0(none_of("&"))(i)?;
    let (i, nmls) = many0(|i| parse_namelist(namelist_spec, i))(i)?;
    if i.len() == 0 {
        Ok((
            i,
            NamelistFile {
                namelists: nmls
                    .into_iter()
                    .filter(|s| s.is_some())
                    .map(|s| s.unwrap())
                    .collect(),
            },
        ))
    } else {
        Err(nom::Err::Error(error_position!(
            i,
            nom::error::ErrorKind::Eof
        )))
    }
}

// TODO: add source location
fn parse_namelist<'a, 'b>(
    namelist_spec: &'a NamelistSpec,
    i: &'b [u8],
) -> IResult<&'b [u8], Option<Namelist>> {
    let (i, _) = char('&')(i)?;
    let (i, nml_name) = parse_nml_name(i)?;
    let group_spec_opt = namelist_spec.get(&nml_name);
    // TODO: namelists only start when there is only whitespace between the
    // start of the line and the ampersand.
    match group_spec_opt {
        None => {
            // skip to the end of an unrecognised namelist, but what if it's the last one? then just fail.
            let (i, _) = many0(none_of("&"))(i)?;
            Ok((i, None))
        }
        Some(group_spec) => {
            let (i, _) = multispace0(i)?;
            let (i, params) =
                separated_list(parse_comma_sep, |i| parse_parameter(&group_spec, i))(i)?;
            // Additional seperators at the end are fine
            let (i, _) = opt(parse_comma_sep)(i)?;
            let (i, _) = alt((char('/'), peek(char('&'))))(i)?;
            // TODO: how should we treat comments? Here we just skip to the next &
            let (i, _) = many0(none_of("&"))(i)?;
            let nml = Namelist {
                name: nml_name,
                parameters: params,
            };
            Ok((i, Some(nml)))
        }
    }
}

fn namelist_start( i: &[u8]) -> IResult<&[u8], ()> {
    let (i, _) = char('\n')(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = peek(char('&'))(i)?;
    Ok((i, ()))
}

pub fn parse_parameter<'a, 'b>(
    group_spec: &'a GroupSpec,
    i: &'b [u8],
) -> IResult<&'b [u8], Parameter> {
    let (i, name) = parameter_name(i)?;
    let name = name.to_uppercase();
    // TODO: we need to make sure the group spec is upper case to
    let parameter_spec =
        group_spec
            .get(&name)
            .expect(&format!("no spec for [{}]: \"{}\"", name.len(), name));
    // TODO: positional parameters
    let (i, _) = multispace0(i)?;
    let (i, pos) = opt(param_pos)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = char('=')(i)?;
    let (i, _) = multispace0(i)?;
    let (i, value) = match parameter_spec {
        ParameterSpec::Atom(atom_spec) => {
            let (i, atom) = parse_parameter_value_atom(*atom_spec, i)?;
            (i, ParameterValue::Atom(atom))
        }
        ParameterSpec::Array(atom_spec) => {
            let pos = pos.unwrap_or(ParamPos::OneDim(Range::Numberless));
            let (i, array) = parse_parameter_value_array(*atom_spec, pos, i)?;
            (i, ParameterValue::Array(pos, array))
        }
    };
    Ok((i, Parameter { name, value }))
}

fn parse_parameter_value_atom(
    parameter_spec_atom: ParameterSpecAtom,
    i: &[u8],
) -> IResult<&[u8], ParameterValueAtom> {
    let (i, value) = match parameter_spec_atom {
        ParameterSpecAtom::String => {
            let (i, s) = quoted_string(i)?;
            (i, s.into())
        }
        ParameterSpecAtom::Double => {
            let (i, s) = parse_double(i)?;
            (i, s.into())
        }
        ParameterSpecAtom::Int => {
            let (i, s) = parse_int(i)?;
            (i, s.into())
        }
        ParameterSpecAtom::Bool => {
            let (i, s) = boolean(i)?;
            (i, s.into())
        }
    };
    Ok((i, value))
}

/// If the atom is followed by an '=' then we should bail
fn parse_parameter_value_atom_no_equals(
    parameter_spec_atom: ParameterSpecAtom,
    i: &[u8],
) -> IResult<&[u8], ParameterValueAtom> {
    let (i, value) = match parameter_spec_atom {
        ParameterSpecAtom::String => {
            let (i, s) = quoted_string(i)?;
            (i, s.into())
        }
        ParameterSpecAtom::Double => {
            // println!("parsing double from: {:?}", i);
            let (i, s) = parse_double(i)?;
            (i, s.into())
        }
        ParameterSpecAtom::Int => {
            let (i, s) = parse_int(i)?;
            (i, s.into())
        }
        ParameterSpecAtom::Bool => {
            let (i, s) = boolean(i)?;
            (i, s.into())
        }
    };
    // println!("i: {:?}", i);
    // println!("parsed: {:?}", value);
    let (_, c_opt) = opt(preceded(
        multispace0,
        preceded(opt(param_pos), preceded(multispace0, char('='))),
    ))(i)?;
    match c_opt {
        Some(_c) => Err(nom::Err::Error((i, nom::error::ErrorKind::Char))), //nom::error::VerboseErrorKind::Char(c)))),
        None => Ok((i, value)),
    }
}

fn parse_parameter_value_array(
    parameter_spec_atom: ParameterSpecAtom,
    pos: ParamPos,
    i: &[u8],
) -> IResult<&[u8], Vec<ParameterValueAtom>> {
    // Get the length of the array we are parsing
    // let n_values = pos.len();
    // println!("n_values: {}", n_values);
    // let (i, value) = count(
    //     terminated(
    //         |i| parse_parameter_value_atom(parameter_spec_atom, i),
    //         parse_comma_sep,
    //     ),
    //     n_values,
    // )(i)?;
    let (i, value) = separated_list(parse_comma_sep, |i| {
        parse_parameter_value_atom_no_equals(parameter_spec_atom, i)
    })(i)?;
    Ok((i, value))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom;
    // use nom::
    use nom::error::ErrorKind::NoneOf;
    use std::error::Error;
    // In these tests Ok(remaining, result) is used to make sure that we have
    // consumed the input we expect to consume.

    #[test]
    fn boolean_examples() {
        assert_eq!(boolean(b"t"), Ok((&[][..], true)));
        assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    }

    #[test]
    fn int_examples() {
        assert_eq!(parse_int(b"-2"), Ok((&[][..], -2)));
        assert_eq!(parse_int(b"60."), Ok((&[][..], 60)));
        // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    }

    #[test]
    fn double_examples() {
        assert_eq!(parse_double(b"1E13"), Ok((&[][..], 1E13)));
        assert_eq!(parse_double(b"2.75E12"), Ok((&[][..], 2.75E12)));
        // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    }

    #[test]
    fn string_examples() {
        assert_eq!(
            quoted_string(b"\'hello\'"),
            Ok((&[][..], String::from("hello")))
        );
        // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    }

    #[test]
    fn parameter_name_examples() {
        assert_eq!(parameter_name(b"speed"), Ok((&[][..], "speed".to_string())));
        assert_eq!(parameter_name(b"s"), Ok((&[][..], "s".to_string())));
        // An empty string is not parsed
        assert_eq!(
            parameter_name(b""),
            Err(nom::Err::Error((b"".as_ref(), NoneOf)))
        );
        // Parameter name cannot start with a number
        assert_eq!(
            parameter_name(b"2speed"),
            Err(nom::Err::Error((b"2speed".as_ref(), NoneOf)))
        );
    }

    #[test]
    fn parameter_examples() {
        use std::collections::HashMap;
        let mut group_spec = HashMap::new();
        group_spec.insert(
            "TEMPERATURE".to_string(),
            ParameterSpec::Atom(ParameterSpecAtom::Double),
        );
        assert_eq!(
            parse_parameter(&group_spec, b"TEMPERATURE=273"),
            Ok((
                &[][..],
                Parameter {
                    name: "TEMPERATURE".to_string(),
                    value: 273_f64.into()
                }
            ))
        );
        // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    }

    #[test]
    fn range_examples() {
        use std::collections::HashMap;
        assert_eq!(
            param_pos(b"(1:2)"),
            Ok((&[][..], ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64))))
        );
        // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    }

    #[test]
    fn array_examples() {
        use std::collections::HashMap;
        let mut group_spec = HashMap::new();
        group_spec.insert(
            "TEMPERATURES".to_string(),
            ParameterSpec::Array(ParameterSpecAtom::Double),
        );
        assert_eq!(
            parse_parameter(&group_spec, b"TEMPERATURES(1:2)=273, 274"),
            Ok((
                &[][..],
                Parameter {
                    name: "TEMPERATURES".to_string(),
                    value: ParameterValue::Array(
                        ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
                        vec![273_f64.into(), 274_f64.into()]
                    )
                }
            ))
        );
        assert_eq!(
            parse_parameter(&group_spec, b"TEMPERATURES(1:2)=273 274"),
            Ok((
                &[][..],
                Parameter {
                    name: "TEMPERATURES".to_string(),
                    value: ParameterValue::Array(
                        ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
                        vec![273_f64.into(), 274_f64.into()]
                    )
                }
            ))
        );
        assert_eq!(
            parse_parameter(&group_spec, b"TEMPERATURES(1)=273"),
            Ok((
                &[][..],
                Parameter {
                    name: "TEMPERATURES".to_string(),
                    value: ParameterValue::Array(
                        ParamPos::OneDim(Range::SingleNumber(1_u64)),
                        vec![273_f64.into()]
                    )
                }
            ))
        );
        // assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        // assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        // assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        // assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        // assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        // assert_eq!(boolean(b".TRUE., "), Ok((b", ".as_ref(), true)));
    }

    #[test]
    fn namelist_examples() {
        use std::collections::HashMap;
        let mut group_spec = HashMap::new();
        group_spec.insert(
            "TEMPERATURES".to_string(),
            ParameterSpec::Array(ParameterSpecAtom::Double),
        );
        let mut namelist_spec: NamelistSpec = HashMap::new();
        namelist_spec.insert("HEAD".to_string(), group_spec);
        let expected = Some(Namelist {
            name: "HEAD".to_string(),
            parameters: vec![Parameter {
                name: "TEMPERATURES".to_string(),
                value: ParameterValue::Array(
                    ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
                    vec![273_f64.into(), 274_f64.into()],
                ),
            }],
        });
        assert_eq!(
            parse_namelist(&namelist_spec, b"&HEAD TEMPERATURES(1:2)=273, 274 /"),
            Ok((&[][..], expected))
        );
    }

    // #[test]
    // fn namelistfile_examples() {
    //     use std::collections::HashMap;
    //     let mut group_spec = HashMap::new();
    //     group_spec.insert(
    //         "TEMPERATURES".to_string(),
    //         ParameterSpec::Array(ParameterSpecAtom::Double),
    //     );
    //     let mut namelist_spec: NamelistSpec = HashMap::new();
    //     namelist_spec.insert("HEAD".to_string(), group_spec);
    //     let expected = Namelist {
    //         name: "HEAD".to_string(),
    //         parameters: vec![Parameter {
    //             name: "TEMPERATURES".to_string(),
    //             value: ParameterValue::Array(
    //                 ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
    //                 vec![273_f64.into(), 274_f64.into()],
    //             ),
    //         }],
    //     };
    //     assert_eq!(
    //         parse_namelist(&namelist_spec, b"&HEAD TEMPERATURES(1:2)=273, 274 /"),
    //         Ok((&[][..], expected))
    //     );
    // }
    #[test]
    fn float_check() {
        assert_eq!(parse_double(&b"1.1"[..]), Ok((&b""[..], 1.1)));
        assert_eq!(parse_double(&b"EX"[..]), Err(nom::Err::Error((&b"EX"[..], nom::error::ErrorKind::OneOf))));
        assert_eq!(parse_double(&b"123E-02"[..]), Ok((&b""[..], 1.23)));
        assert_eq!(parse_double(&b"123K-01"[..]), Ok((&b"K-01"[..], 123.0)));
        assert_eq!(parse_double(&b"abc"[..]), Err(nom::Err::Error((&b"abc"[..], nom::error::ErrorKind::OneOf))));

    }
}
