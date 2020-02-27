use std::collections::HashMap;
use std::io::Read;
use std::io::{BufRead, BufReader};
use std::convert::{TryFrom, TryInto};
use log::debug;
use std::str::FromStr;
use nom::{
    branch::alt,
    character::complete::{
        alphanumeric1, anychar, char, none_of,
    },
    combinator::{peek},
    multi::{many0, many1},
    IResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct NamelistFile {
    pub namelists: Vec<Namelist>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Namelist {
    pub name: String,
    pub parameters: HashMap<String, ParameterValue>,
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
            let parameter_values: ParameterValue = ParameterValue::from(pos, param_tokens).unwrap();
            nml.parameters.entry(param_name.clone())
                .and_modify(|param| {
                    combine_param(param, &parameter_values);
                })
                .or_insert(parameter_values);
        }
        Ok(nml)
    }
}

fn combine_param(p1: &mut ParameterValue, p2: &ParameterValue) {
    match (p1, p2) {
        // If it's an atom, just replace it. If we're replacing an atom with an
        // array, this shouldn't happen, but just overwrite.
        (p1@ParameterValue::Atom(_), _) => p1.clone_from(p2),
        // An atom replacing an array should also not happen, but do it.
        (p1@ParameterValue::Array(_), ParameterValue::Atom(_)) => p1.clone_from(p2),
        // If it's an array, combine and overwrite.
        (ParameterValue::Array(ref mut original_array),ParameterValue::Array(ref new_array)) => combine_arrays(original_array, new_array),
    }
}

/// Add the elements of a2 to a1, overwriting where necessary
/// TODO: use entry API
fn combine_arrays(a1: &mut HashMap<Vec<i64>, String>, a2: &HashMap<Vec<i64>, String>) {
    for (k,v) in a2.iter() {
        a1.insert(k.clone(),v.clone());
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
    // pub pos: Option<ParamPos>,
    pub value: ParameterValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterValue {
    Atom(String),
    Array(HashMap<Vec<i64>,String>),
}

impl ParameterValue {

    fn from(pos: Option<ParamPos>, tokens: Vec<Token>) -> Result<Self, ()> {
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
                let mut value_map: HashMap<Vec<i64>, String> = HashMap::new();
                let indices = pos.map(|p| p.iter()).unwrap_or(ParamPos::default_iter());
                for (is, value) in indices.zip(vals.into_iter()) {
                    value_map.insert(is.into_iter().map(|x| x as i64).collect(), value);
                }
                Ok(ParameterValue::Array(value_map))
            },
        }
    }
}

impl TryFrom<ParameterValue> for bool {
    type Error = ();

    fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
        match pv {
            ParameterValue::Atom(s) => {
                let s_val: NmlBool = s.parse().unwrap();
                Ok(s_val.0)
            },
            ParameterValue::Array(_) => panic!("expected bool, not array"),
        }
    }
}



impl TryFrom<ParameterValue> for String {
    type Error = ();

    fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
        match pv {
            ParameterValue::Atom(s) => {
                let s_val: NmlString = s.parse().unwrap();
                Ok(s_val.0)
            },
            ParameterValue::Array(_) => panic!("expected string, not array"),
        }
    }
}


impl TryFrom<ParameterValue> for u64 {
    type Error = ();

    fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
        match pv {
            ParameterValue::Atom(s) => {
                let s_val: NmlUint = s.parse().unwrap();
                Ok(s_val.0)
            },
            ParameterValue::Array(_) => panic!("expected unsigned integer, not array"),
        }
    }
}

impl TryFrom<ParameterValue> for i64 {
    type Error = ();

    fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
        match pv {
            ParameterValue::Atom(s) => {
                let s_val: NmlInt = s.parse().unwrap();
                Ok(s_val.0)
            },
            ParameterValue::Array(_) => panic!("expected signed integer, not array"),
        }
    }
}

impl TryFrom<ParameterValue> for f64 {
    type Error = ();

    fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
        match pv {
            ParameterValue::Atom(s) => {
                let s_val: NmlFloat = s.parse().unwrap();
                Ok(s_val.0)
            },
            ParameterValue::Array(_) => panic!("expected float, not array"),
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

#[derive(Clone, Debug, PartialEq)]
pub struct NmlBool(bool);

impl FromStr for NmlBool {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim_matches('.');
        if s.starts_with("t") {
            return Ok(NmlBool(true));
        }
        if s.starts_with("T") {
            return Ok(NmlBool(true));
        }
        if s.starts_with("f") {
            return Ok(NmlBool(false));
        }
        if s.starts_with("F") {
            return Ok(NmlBool(false));
        }
        Err(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NmlInt(i64);

impl FromStr for NmlInt {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(NmlInt(s.parse().unwrap()))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NmlUint(u64);

impl FromStr for NmlUint {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(NmlUint(s.parse().unwrap()))
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct NmlFloat(f64);

impl FromStr for NmlFloat {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(NmlFloat(s.parse().unwrap()))
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct NmlString(String);

impl FromStr for NmlString {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // TODO: fail if it's not wrapped in single quotes (and only once)
        Ok(NmlString(s.trim_matches('\'').parse().unwrap()))
    }
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

    pub fn iter(&self) -> PosIter {
        match self {
            ParamPos::OneDim(Range::Numberless) => PosIter {
                indices: vec![1],
                counting_index: 0,
                max: None,
            },
            ParamPos::OneDim(Range::SingleNumber(n)) => PosIter {
                indices: vec![*n as usize],
                counting_index: 0,
                max: Some(*n as usize),
            },
            ParamPos::OneDim(Range::TwoNumber(n1, n2)) => PosIter {
                indices: vec![*n1 as usize],
                counting_index: 0,
                max: Some(*n2 as usize),
            },
            ParamPos::TwoDim(Range::Numberless, Range::Numberless) => panic!("single dimensions at a time"),
            ParamPos::TwoDim(Range::TwoNumber(_,_), Range::Numberless) => panic!("single dimensions at a time"),
            ParamPos::TwoDim(Range::Numberless, Range::TwoNumber(_,_)) => panic!("single dimensions at a time"),
            ParamPos::TwoDim(Range::TwoNumber(_,_), Range::TwoNumber(_,_)) => panic!("single dimensions at a time"),
            ParamPos::TwoDim(Range::SingleNumber(n), Range::Numberless) => PosIter {
                indices: vec![*n as usize, 1],
                counting_index: 1,
                max: None,
            },
            ParamPos::TwoDim(Range::Numberless, Range::SingleNumber(n)) => PosIter {
                indices: vec![1, *n as usize],
                counting_index: 0,
                max: None,
            },
            ParamPos::TwoDim(Range::SingleNumber(n), Range::TwoNumber(start,end)) => PosIter {
                indices: vec![*n as usize, *start as usize],
                counting_index: 1,
                max: Some(*end as usize),
            },
            ParamPos::TwoDim(Range::TwoNumber(start,end), Range::SingleNumber(n)) => PosIter {
                indices: vec![*start as usize, *n as usize],
                counting_index: 0,
                max: Some(*end as usize),
            },
            ParamPos::TwoDim(Range::SingleNumber(n1), Range::SingleNumber(n2)) => PosIter {
                indices: vec![*n1 as usize, *n2 as usize],
                counting_index: 0,
                max: Some(*n1 as usize),
            },
        }
    }

    pub fn default_iter() -> PosIter {
        PosIter {
            indices: vec![1],
            counting_index: 0,
            max: None
        }
    }
}

pub struct PosIter {
    indices: Vec<usize>,
    counting_index: usize,
    max: Option<usize>
}

impl Iterator for PosIter {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(max) = self.max {
            if *self.indices.get(self.counting_index).unwrap() > max {
                return None;
            }
        }
        let item = self.indices.clone();
        let i = self.indices.get_mut(self.counting_index).unwrap();
        *i += 1;
        Some(item)
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
                let (i, tokens) = tokenize_nml(line).expect("could not tokenize");
                // This assert is no longer true, as the line may still contain
                // comments. It cannot contain a new namelist as a namelist must
                // be the first thing on a line.
                // assert_eq!(i,"");
                self.buf.clear();
                // Ignore everything after RightSlash
                let mut trimmed_tokens = Vec::new();
                for token in tokens.into_iter() {
                    if token == Token::RightSlash {
                        trimmed_tokens.push(token);
                        break;
                    } else {
                        trimmed_tokens.push(token);
                    }
                }
                let mut tokens: Vec<Token> = trimmed_tokens;
                {
                    let current_nml = self.current_nml.as_mut().expect("could not add to no current nml");
                    current_nml.1.append(&mut tokens);
                }
                if self.current_nml.clone().unwrap().1.last() == Some(&Token::RightSlash) {
                    let current_nml = self.current_nml.clone().unwrap();
                    self.current_nml = None;
                    break Some(current_nml.try_into().unwrap());
                }
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
    let mut tokens = Vec::new();
    let mut i = i;
    loop {
        if i == "" {
            break;
        }
        let (adj, tok) = parse_token(i)?;
        i = adj;
        let end = tok == Token::RightSlash;
        tokens.push(tok);
        if i == "" || end {
            break;
        }
    }
    Ok((i, tokens))
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
            let (i, mut others) = many0(nom::character::complete::none_of("'"))(i)?;
            let mut chars = vec!['\''];
            chars.append(&mut others);
            let (i, end_c) = char('\'')(i)?;
            chars.append(&mut vec![end_c]);
            let string: String = chars.into_iter().collect();
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

    #[test]
    fn boolean_examples() {
        assert_eq!("t".parse(), Ok(NmlBool(true)));
        assert_eq!("T".parse(), Ok(NmlBool(true)));
        assert_eq!("f".parse(), Ok(NmlBool(false)));
        assert_eq!("F".parse(), Ok(NmlBool(false)));
        assert_eq!(".FALSE.".parse(), Ok(NmlBool(false)));
        assert_eq!(".TRUE.".parse(), Ok(NmlBool(true)));
    }

    #[test]
    fn int_examples() {
        assert_eq!("-2".parse(), Ok(NmlInt(-2)));
        assert_eq!("60".parse(), Ok(NmlInt(60)));
    }

    #[test]
    fn double_examples() {
        assert_eq!("1E13".parse(), Ok(NmlFloat(1e13)));
        assert_eq!("2.75E12".parse(), Ok(NmlFloat(2.75e12)));
    }

    #[test]
    fn string_examples() {
        assert_eq!(
            "\'hello\'".parse(),
            Ok(NmlString("hello".to_string()))
        );
    }

    #[test]
    fn float_check() {
        assert_eq!("1.1".parse(), Ok(NmlFloat(1.1)));
        assert_eq!("123E-02".parse(), Ok(NmlFloat(1.23)));
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

    #[test]
    fn regression_1() {
        let input = "&SURF ID='Surface02', RGB=146,202,166, BACKING='VOID', MATL_ID(1,1)='STEEL', MATL_MASS_FRACTION(1,1)=1.0, THICKNESS(1)=0.003/\n! DUMP: NFRAMES: Output is dumped every 1.00 s";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        for nml in parser {
            println!("NML: {:?}", nml);
        }
    }

    #[test]
    fn regression_2() {
        let input = "&OBST XB=19,20,-1,1, 0,100 /  Left Facade";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        for nml in parser {
            println!("NML: {:?}", nml);
        }
    }
}
