#[macro_use]
extern crate nom;
use std::collections::HashMap;
use std::io::Read;
use std::io::{BufRead, BufReader};
use std::convert::TryFrom;

// TODO: need to change this to a line-based parse to handle comments etc.
use nom::{
    branch::alt,
    bytes::complete::is_a,
    character::complete::{
        alphanumeric1, anychar, char, crlf, digit1, multispace0, newline, none_of, one_of, space0,
    },
    combinator::{opt, peek, value},
    multi::{many0, many1, many_till, separated_list},
    number::complete::double,
    sequence::preceded,
    IResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct NamelistFile {
    pub namelists: Vec<Namelist>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Namelist {
    pub name: String,
    pub parameters: HashMap<String, Parameter>,
}

impl TryFrom<(String, Vec<Token>)> for Namelist {
    type Error = &'static str;

    fn try_from(vals: (String, Vec<Token>)) -> Result<Self, Self::Error> {
        let mut nml: Self = Self {
            name: vals.0.clone(),
            parameters: HashMap::new(),
        };
        let tokens = vals.1;
        let eq_split = EqualsSplitter::new(tokens.into_iter());
        for (param_name, pos_tokens, param_tokens) in eq_split {
            let param_name = if let Token::Str(s) = param_name {
                s
            } else {
                panic!("{:?} not a valid param name", param_name)
            };
            println!("pair: {}: {:?} - {:?}", param_name, pos_tokens, param_tokens);

        }
        todo!()
    }
}

struct EqualsSplitter {
    tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
    prev: Option<Token>,
}

impl<'a> EqualsSplitter {
    pub fn new(tokens: std::vec::IntoIter<Token>) -> Self {
        Self {
            tokens: tokens.peekable(),
            prev: None,
        }
    }
}

impl<'a> Iterator for EqualsSplitter {
    type Item = (Token, Vec<Token>, Vec<Token>);

    fn next(&mut self) -> Option<Self::Item> {
        let mut param_name = self.prev.clone();
        self.prev = None;
        if param_name.is_none() {
            param_name = self.tokens.next().clone();
        }
        if param_name.is_none() || param_name == Some(Token::RightSlash) {
            return None;
        }
        println!("param_name: {:?}", param_name);
        let mut pos_tokens = Vec::new();
        if let Some(&Token::LeftBracket) = self.tokens.peek() {
            self.tokens.next().unwrap();
            loop {
                let t = self.tokens.next().unwrap();
                if t == Token::RightBracket {
                    break;
                } else {
                    pos_tokens.push(t);
                }
            }

        }
        match self.tokens.next().unwrap() {
            Token::Equals => {
                // Now we have the parameter name and equals, keep adding tokens
                // until we get to the next equals or the end slash
                let mut param_tokens = Vec::new();
                loop {
                    if let Some(&Token::Equals) = self.tokens.peek() {
                        println!("found equals, current prev: {:?}", self.prev);
                        // We have found the next equals, so we return what we have.
                        return Some((param_name.unwrap(), pos_tokens, param_tokens));
                    }
                    let token = self.tokens.next();
                    if token.is_none() {
                        return Some((param_name.unwrap(), pos_tokens, param_tokens));
                    }
                    let token = token.unwrap();
                    println!("processing token: {:?}", token);
                    if let Some(prev) = self.prev.clone() {
                        self.prev = Some(token.clone());
                        param_tokens.push(prev);
                    } else {
                        self.prev = Some(token.clone());
                    }

                }
                return Some((param_name.unwrap(), pos_tokens, param_tokens));
            },
            e => panic!("expected '=' found {:?}", e),
        }
    }

}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub value: ParameterValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterValue {
    Atom(ParameterValueAtom),
    Array(ParameterArray),
}

/// Currently optimised for very sparse arrays (mainly for simplicity of
/// implementation).
#[derive(Clone, Debug, PartialEq)]
pub struct ParameterArray {
    pub pos: ParamPos,
    pub values: HashMap<Vec<i64>, ParameterValueAtom>,
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
/// non-whitespace characters. It may also prepended by a period.
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

pub fn parse_nml_name_any<T>(i: T) -> IResult<T, String>
where
    T: nom::InputTakeAtPosition + nom::InputIter,
    <T as nom::InputTakeAtPosition>::Item: nom::AsChar,
    <T as nom::InputIter>::Item: nom::AsChar + Copy,
    {
    use nom::AsChar;
    let (i, name): (T,T) = alphanumeric1(i)?;
    let mut v: Vec<char> = Vec::new();
    for c in name.iter_elements() {
        v.push(c.as_char())
    }
    let name: String = v.into_iter().collect();
    Ok((
        i,
        name
    ))
}

pub fn parse_nml_name_str(i: &str) -> IResult<&str, String> {
    let (i, name) = alphanumeric1(i)?;
    Ok((
        i,
        String::from(name)
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
                "In a two-dimensional position parameter, one dimensions must be a single number"
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

/// Parses a comma, possibly surrounded by spaces, or possibly EOF
pub fn parse_comma_sep_no_newline(i: &[u8]) -> IResult<&[u8], ()> {
    let (i, _) = many1(parse_comma_sep_no_newline_single)(i)?;
    Ok((i, ()))
}

pub fn parse_comma_sep_no_newline_single(i: &[u8]) -> IResult<&[u8], ()> {
    let (i, _) = peek(one_of(" \t,"))(i)?;
    let (i, _) = space0(i)?;
    let (i, _) = opt(char(','))(i)?;
    let (i, _) = space0(i)?;
    Ok((i, ()))
}

pub fn parse_namelist_file<'a, 'b>(
    namelist_spec: &'a NamelistSpec,
    i: &'b [u8],
) -> IResult<&'b [u8], NamelistFile> {
    // Skip to the first namelist
    let (i, _) = multispace0(i)?;
    let (i, c_opt) = opt(peek(char('&')))(i)?;
    let (i, _) = if !c_opt.is_some() {
        value((), many_till(anychar, namelist_start))(i)?
    } else {
        (i, ())
    };
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

pub struct NmlParser<R> {
    reader: BufReader<R>,
    // in_nml: bool,
    current_nml: Option<(String, Vec<Token>)>,
    buf: String,
    namelists: Vec<(String, Vec<Token>)>,
    // We are on the last iteration.
    last: bool,
}

impl<R: Read> NmlParser<R> {
    pub fn new(input: R) -> Self {
        NmlParser {
            reader: BufReader::new(input),
            // in_nml: false,
            current_nml: None,
            buf: String::new(),
            namelists: Vec::new(),
            last: false,
        }
    }
}

impl<R: Read> Iterator for NmlParser<R> {
    type Item = (String, Vec<Token>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.last == true {
            return None;
        }
        // Iterate through all the lines, parsing as we go. Each loop iteration is
        // for a single namespace.
        loop {
            // If we already have a line in the buffer, use that, else get a new
            // one.
            if self.buf.len() == 0 {
                // Get a new line.
                let n = self.reader.read_line(&mut self.buf).expect("read_line failed");
                if n == 0 {
                    // There is no data left. If we have a current nml, we
                    // return that, otherwise we just return None.
                    self.last = true;
                    break self.current_nml.clone();
                }
            }
            let mut line: &str = self.buf.trim();
            // println!("line: {}", line);
            // If the line (after whitespace) begins with an ampersand, it is a new
            // namelist.

            if line.starts_with("&") {
                // If we currently have an nml we are working on, return that.
                // We need to make sure that on the next iteration we are in the
                // right position to get the next nml. This includes setting the
                // "current_nml" buffer to None, and leaving the line buffer
                // as-is.
                if self.current_nml.is_some() {
                    let current_nml = self.current_nml.clone().expect("no current nml");
                    self.current_nml = None;
                    break Some(current_nml);
                }
                // First, skip the ampersand character.
                let i = &line[1..];
                // Parse the type of NML.
                let (i, nml_type) = parse_nml_name_str(i).expect("invalid namelist group");
                // Create an empty namelist to be parsing.
                self.current_nml = Some((nml_type, Vec::new()));
                // Make sure to set the start the start of the tokens.
                line = i;
            }

            if self.current_nml.is_some() {
                // Tokenize the rest of the line.
                let (i, mut tokens) = tokenize_nml(line).expect("could not tokenize");
                assert_eq!(i,"");
                let current_nml = self.current_nml.as_mut().expect("could not add to no current nml");
                current_nml.1.append(&mut tokens);
                // If the line does not start with '&' it is either empty or a comment,
                // so we just continue looping.
            }
            self.buf.clear();
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LeftBracket,
    RightBracket,
    Equals,
    Colon,
    Comma,
    RightSlash,
    /// Some variable string that forms a token. Currently this could also
    /// include numbers.
    Str(String),
}

pub fn tokenize_nml(i: &str) -> IResult<&str, Vec<Token>> {
    many0(parse_token)(i)
}

pub fn parse_token(i: &str) -> IResult<&str, Token> {
    let i = i.trim();
    let (i, first_char) = anychar(i)?;
    match first_char {
        '(' => Ok((i, Token::LeftBracket)),
        ')' => Ok((i, Token::RightBracket)),
        ':' => Ok((i, Token::Colon)),
        '=' => Ok((i, Token::Equals)),
        ',' => Ok((i, Token::Comma)),
        '/' => Ok((i, Token::RightSlash)),
        '\'' => {
            // We have begun a quoted string. Fortran (FDS at least) does not
            // support escapes, so we can just look for the next single quote.
            let (i, chars) = many0(nom::character::complete::none_of("'"))(i)?;
            let string: String = chars.into_iter().collect();
            let (i, _) = char('\'')(i)?;
            Ok((i, Token::Str(string)))
        },
        c => {
            // We have some other char and must continue until we reach some
            // other type [():'=] or whitespace.
            let (i, mut others) = many0(nom::character::complete::none_of(" \t\r\n():=',/"))(i)?;
            let mut chars = vec![c];
            chars.append(&mut others);
            let string: String = chars.into_iter().collect();
            Ok((i, Token::Str(string)))
        }
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
            let (i, _) = many_till(anychar, namelist_start)(i)?;
            Ok((i, None))
        }
        Some(group_spec) => {
            let (i, _) = multispace0(i)?;
            let (i, params) =
                separated_list(parse_comma_sep, |i| parse_parameter(&group_spec, i))(i)?;
            // Additional seperators at the end are fine
            let (i, _) = many_till(
                alt((
                    parse_comma_sep_no_newline,
                    value((), crlf),
                    value((), newline),
                )),
                alt((
                    namelist_start,
                    end_of_file,
                    ampersand_eof,
                    value((), char('/')),
                )),
            )(i)?;
            let (i, _) = many_till(anychar, alt((namelist_start, end_of_file)))(i)?;
            let mut params_map = HashMap::new();
            for param in params {
                // TODO: what about arrays
                params_map.entry(param.name.clone())
                    .and_modify(|a| combine_param(a,&param))
                    .or_insert(param);
            }
            let nml = Namelist {
                name: nml_name,
                parameters: params_map,
            };
            Ok((i, Some(nml)))
        }
    }
}

fn combine_param(p1: &mut Parameter, p2: &Parameter) {
    match (&mut p1.value, &p2.value) {
        // If it's an atom, just replace it. If we're replacing an atom with an
        // array, this shouldn't happen, but just overwrite.
        (ParameterValue::Atom(_), _) => p1.clone_from(p2),
        // An atom replacing an array should also not happen, but do it.
        (ParameterValue::Array(_), ParameterValue::Atom(_)) => p1.clone_from(p2),
        // If it's an array, combine and overwrite.
        (ParameterValue::Array(ref mut original_array),ParameterValue::Array(ref new_array)) => combine_arrays(original_array, new_array),
    }
}

/// Add the elements of a2 to a1, overwriting where necessary
fn combine_arrays(a1: &mut ParameterArray, a2: &ParameterArray) {
    for (k,v) in a2.values.iter() {
        a1.values.insert(k.clone(),v.clone());
    }
}

/// Special case that apparently needs handling.
fn ampersand_eof(i: &[u8]) -> IResult<&[u8], ()> {
    let (i, _) = char('&')(i)?;
    let (i, _) = multispace0(i)?;
    end_of_file(i)
}

fn end_of_file(i: &[u8]) -> IResult<&[u8], ()> {
    if i.len() == 0 {
        Ok((i, ()))
    } else {
        Err(nom::Err::Error(error_position!(
            i,
            nom::error::ErrorKind::Eof
        )))
    }
}

fn namelist_start(i: &[u8]) -> IResult<&[u8], ()> {
    let (i, _) = opt(char('\r'))(i)?;
    let (i, _) = newline(i)?;
    let (i, _) = space0(i)?;
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
            (i, ParameterValue::Array(ParameterArray{pos:pos, values:array}))
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
    _pos: ParamPos,
    i: &[u8],
) -> IResult<&[u8], HashMap<Vec<i64>, ParameterValueAtom>> {
    let (i, values) = separated_list(parse_comma_sep, |i| {
        parse_parameter_value_atom_no_equals(parameter_spec_atom, i)
    })(i)?;
    let mut value_map = HashMap::new();
    // TODO: doesn't handle complex indexing
    let mut index = 1_i64;
    for value in values {
        value_map.insert(vec![index], value);
        index += 1;
    }
    Ok((i, value_map))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom;
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
        let mut value_map = HashMap::new();
        value_map.insert(vec![1], 273_f64.into());
        value_map.insert(vec![2], 274_f64.into());
        assert_eq!(
            parse_parameter(&group_spec, b"TEMPERATURES(1:2)=273, 274"),
            Ok((
                &[][..],
                Parameter {
                    name: "TEMPERATURES".to_string(),
                    value: ParameterValue::Array(
                        ParameterArray {
                            pos: ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
                            values: value_map.clone(),
                        }
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
                        ParameterArray {
                            pos: ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
                            values: value_map
                        }
                    )
                }
            ))
        );
        let mut value_map = HashMap::new();
        value_map.insert(vec![1], 273_f64.into());
        assert_eq!(
            parse_parameter(&group_spec, b"TEMPERATURES(1)=273"),
            Ok((
                &[][..],
                Parameter {
                    name: "TEMPERATURES".to_string(),
                    value: ParameterValue::Array(
                        ParameterArray {
                            pos: ParamPos::OneDim(Range::SingleNumber(1_u64)),
                            values: value_map
                        }
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
        let mut value_map = HashMap::new();
        value_map.insert(vec![1], 273_f64.into());
        value_map.insert(vec![2], 274_f64.into());
        let mut params_map = HashMap::new();
        params_map.insert("TEMPERATURES".to_string(), Parameter {
                name: "TEMPERATURES".to_string(),
                value: ParameterValue::Array(
                    ParameterArray {
                        pos: ParamPos::OneDim(Range::TwoNumber(1_u64, 2_u64)),
                        values: value_map,
                    }
                ),
            });
        let expected = Some(Namelist {
            name: "HEAD".to_string(),
            parameters: params_map,
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
        assert_eq!(
            parse_double(&b"EX"[..]),
            Err(nom::Err::Error((&b"EX"[..], nom::error::ErrorKind::OneOf)))
        );
        assert_eq!(parse_double(&b"123E-02"[..]), Ok((&b""[..], 1.23)));
        assert_eq!(parse_double(&b"123K-01"[..]), Ok((&b"K-01"[..], 123.0)));
        assert_eq!(
            parse_double(&b"abc"[..]),
            Err(nom::Err::Error((&b"abc"[..], nom::error::ErrorKind::OneOf)))
        );
    }

    #[test]
    fn simple_tokens() {
        assert_eq!(
            tokenize_nml("TEMPERATURES(1:2)=273, 274"),
            Ok((
                "",
                vec![
                    Token::Str("TEMPERATURES".to_string()),
                    Token::LeftBracket,
                    Token::Str("1".to_string()),
                    Token::Colon,
                    Token::Str("2".to_string()),
                    Token::RightBracket,
                    Token::Equals,
                    Token::Str("273".to_string()),
                    Token::Comma,
                    Token::Str("274".to_string()),
                ]
            ))
        );
    }

    #[test]
    fn nml_iter() {
        use std::convert::TryInto;
        let f = std::fs::File::open("room_fire.fds").expect("could not open test file");
        let parser = NmlParser::new(f);
        for nml in parser {
            println!("NML: {}: {:?}", nml.0, nml.1);
            let namelist: Namelist = nml.try_into().expect("conversion failed");
            println!("NMLD: {:?}", namelist);
        }
    }
}
