use std::collections::HashMap;
use std::io::Read;
use std::io::{BufRead, BufReader};
use std::convert::{TryFrom, TryInto};
use log::debug;

use nom::{
    branch::alt,
    bytes::complete::is_a,
    character::complete::{
        alphanumeric1, anychar, char, digit1, none_of, one_of,
    },
    combinator::{opt, peek},
    multi::{many0, many1},
    number::complete::double,
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
        debug!("tokens: {:?}", tokens);
        let eq_split = EqualsSplitter::new(tokens.into_iter());
        for (param_name, pos_tokens, param_tokens) in eq_split {
            let param_name = if let Token::Str(s) = param_name {
                s
            } else {
                panic!("{:?} not a valid param name", param_name)
            };
            let pos: Option<ParamPos> = if pos_tokens.len() > 0 {
                Some(pos_tokens.try_into().expect("could not parse parampos"))
            } else {
                None
            };
            debug!("pair: {}({:?}): - {:?}", param_name, pos, param_tokens);
            let parameter_values: ParameterValue = param_tokens.try_into().unwrap();
            nml.parameters.insert(param_name.clone(), Parameter {
                name: param_name,
                pos: pos,
                value: parameter_values,
            });
        }
        Ok(nml)
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
        debug!("param_name: {:?}", param_name);
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
                    {
                        if let Some(some_token) = self.tokens.peek() {
                            match some_token {
                                &Token::Equals | Token::LeftBracket => {
                                    debug!("found equals or left bracket, current prev: {:?}", self.prev);
                                    // We have found the next equals, so we return what we have.
                                    return Some((param_name.unwrap(), pos_tokens, param_tokens));
                                }
                                _ => (),
                            }
                        }
                    }
                    let token = self.tokens.next();
                    if token.is_none() {
                        return Some((param_name.unwrap(), pos_tokens, param_tokens));
                    }
                    let token = token.unwrap();
                    debug!("processing token: {:?}", token);
                    if let Some(prev) = self.prev.clone() {
                        self.prev = Some(token.clone());
                        param_tokens.push(prev);
                    } else {
                        self.prev = Some(token.clone());
                    }

                }
            },
            e => panic!("expected '=' found {:?}", e),
        }
    }

}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub pos: Option<ParamPos>,
    pub value: ParameterValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterValue {
    Atom(String),
    Array(Vec<String>),
}

impl TryFrom<Vec<Token>> for ParameterValue {
    type Error = ();

    fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
        match tokens.len() {
            0 => panic!("no tokens"),
            // We have a single value
            1 => Ok(into_parameter_value_atom(tokens[0].clone())),
            // We have many values
            _many => {
                let vals: Vec<String> = tokens.into_iter().filter(|x| x != &Token::Comma).map(|x| match x {
                    Token::Str(s) => s,
                    v =>  panic!("invalid array value: {:?}", v),
                }).collect();
                Ok(ParameterValue::Array(vals))
            },
        }
    }
}

fn into_parameter_value_atom(token: Token) -> ParameterValue {
    match token {
        Token::Str(s) => ParameterValue::Atom(s),
        _ => panic!("invalid atom value"),
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParameterArray {
    pub pos: ParamPos,
    pub values: HashMap<Vec<i64>, String>,
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

impl TryFrom<Vec<Token>> for ParamPos {
    type Error = ();

    fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
        let sections: Vec<&[Token]> = tokens.split(|x| x == &Token::Comma).collect();
        let ranges: Vec<Range> = sections.into_iter().map(|x| x.try_into().unwrap()).collect();
        match ranges.len() {
            // We have a single-dimensional value
            1 => Ok(ParamPos::OneDim(ranges[0])),
            // We have a two-dimensional value
            2 => Ok(ParamPos::TwoDim(ranges[0], ranges[1])),
            // TODO: technically they can have more dimensions
            _ => panic!("Only 1 or 2 dimensions allowed"),
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

impl TryFrom<&[Token]> for Range {
    type Error = ();

    fn try_from(tokens: &[Token]) -> Result<Self, Self::Error> {
        match tokens {
            [Token::Colon] => Ok(Range::Numberless),
            [Token::Str(s)] => Ok(Range::SingleNumber(s.parse().unwrap())),
            [Token::Str(s1), Token::Colon, Token::Str(s2)] => Ok(Range::TwoNumber(s1.parse().unwrap(), s2.parse().unwrap())),
            _ => panic!("too many elements for range"),
        }
    }
}

#[derive(Debug)]
pub struct NmlParser<R> {
    reader: BufReader<R>,
    current_nml: Option<(String, Vec<Token>)>,
    buf: String,
    // We are on the last iteration.
    last: bool,
}

impl<R: Read> NmlParser<R> {
    pub fn new(input: R) -> Self {
        NmlParser {
            reader: BufReader::new(input),
            current_nml: None,
            buf: String::new(),
            last: false,
        }
    }
}

impl<R: Read> Iterator for NmlParser<R> {
    type Item = Namelist;

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
                    break self.current_nml.clone().map(|x| x.try_into().unwrap());
                }
            }
            let mut line: &str = self.buf.trim();
            // debug!("line: {}", line);
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
                    break Some(current_nml.try_into().unwrap());
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

// TODO: add source location
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

#[cfg(test)]
mod tests {
    use super::*;
    use nom;
    use nom::error::ErrorKind::NoneOf;
    // use std::error::Error;
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
        let f = std::fs::File::open("room_fire.fds").expect("could not open test file");
        let parser = NmlParser::new(f);
        for nml in parser {
            println!("NML: {:?}", nml);
        }
    }
}
