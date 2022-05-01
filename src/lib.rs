pub mod namelists;
pub mod tokenizer;

#[derive(Clone, Debug, PartialEq)]
pub struct NamelistFile {
    pub namelists: Vec<Namelist>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Namelist {
    pub tokens: Vec<LocatedToken>, // pub name: String,
                                   //     pub parameters: HashMap<String, ParameterValue>
}

// impl TryFrom<(String, Vec<Token>)> for Namelist {
//     type Error = &'static str;

//     fn try_from(vals: (String, Vec<Token>)) -> Result<Self, Self::Error> {
//         let mut nml: Self = Self {
//             name: vals.0.clone(),
//             parameters: HashMap::new(),
//         };
//         let tokens = vals.1;
//         debug!("tokens: {:?}", tokens);
//         let eq_split = EqualsSplitter::new(tokens.into_iter());
//         for (param_name, pos_tokens, param_tokens) in eq_split {
//             let param_name = if let Token::Str(s) = param_name {
//                 s
//             } else {
//                 panic!("{:?} not a valid param name", param_name)
//             };
//             let pos: Option<ParamPos> = if pos_tokens.len() > 0 {
//                 Some(pos_tokens.try_into().expect("could not parse parampos"))
//             } else {
//                 None
//             };
//             debug!("pair: {}({:?}): - {:?}", param_name, pos, param_tokens);
//             let parameter_values: ParameterValue = ParameterValue::from(pos, param_tokens).unwrap();
//             nml.parameters
//                 .entry(param_name.clone())
//                 .and_modify(|param| {
//                     combine_param(param, &parameter_values);
//                 })
//                 .or_insert(parameter_values);
//         }
//         Ok(nml)
//     }
// }

// fn combine_param(p1: &mut ParameterValue, p2: &ParameterValue) {
//     match (p1, p2) {
//         // If it's an atom, just replace it. If we're replacing an atom with an
//         // array, this shouldn't happen, but just overwrite.
//         (p1 @ ParameterValue::Atom(_), _) => p1.clone_from(p2),
//         // An atom replacing an array should also not happen, but do it.
//         (p1 @ ParameterValue::Array(_), ParameterValue::Atom(_)) => p1.clone_from(p2),
//         // If it's an array, combine and overwrite.
//         (ParameterValue::Array(ref mut original_array), ParameterValue::Array(ref new_array)) => {
//             combine_arrays(original_array, new_array)
//         }
//     }
// }

// /// Add the elements of a2 to a1, overwriting where necessary
// /// TODO: use entry API
// fn combine_arrays(a1: &mut HashMap<Vec<i64>, String>, a2: &HashMap<Vec<i64>, String>) {
//     for (k, v) in a2.iter() {
//         a1.insert(k.clone(), v.clone());
//     }
// }

// struct EqualsSplitter {
//     tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
//     prev: Option<Token>,
// }

// impl<'a> EqualsSplitter {
//     pub fn new(tokens: std::vec::IntoIter<Token>) -> Self {
//         Self {
//             tokens: tokens.peekable(),
//             prev: None,
//         }
//     }
// }

// impl<'a> Iterator for EqualsSplitter {
//     type Item = (Token, Vec<Token>, Vec<Token>);

//     fn next(&mut self) -> Option<Self::Item> {
//         let mut param_name = self.prev.clone();
//         self.prev = None;
//         if param_name.is_none() {
//             param_name = self.tokens.next().clone();
//         }
//         if param_name.is_none() || param_name == Some(Token::RightSlash) {
//             return None;
//         }
//         debug!("param_name: {:?}", param_name);
//         let mut pos_tokens = Vec::new();
//         if let Some(&Token::LeftBracket) = self.tokens.peek() {
//             self.tokens.next().unwrap();
//             loop {
//                 let t = self.tokens.next().unwrap();
//                 if t == Token::RightBracket {
//                     break;
//                 } else {
//                     pos_tokens.push(t);
//                 }
//             }
//         }
//         match self.tokens.next().unwrap() {
//             Token::Equals => {
//                 // Now we have the parameter name and equals, keep adding tokens
//                 // until we get to the next equals or the end slash
//                 let mut param_tokens = Vec::new();
//                 loop {
//                     {
//                         if let Some(some_token) = self.tokens.peek() {
//                             match some_token {
//                                 &Token::Equals | Token::LeftBracket => {
//                                     debug!(
//                                         "found equals or left bracket, current prev: {:?}",
//                                         self.prev
//                                     );
//                                     // We have found the next equals, so we return what we have.
//                                     return Some((param_name.unwrap(), pos_tokens, param_tokens));
//                                 }
//                                 _ => (),
//                             }
//                         }
//                     }
//                     let token = self.tokens.next();
//                     if token.is_none() {
//                         return Some((param_name.unwrap(), pos_tokens, param_tokens));
//                     }
//                     let token = token.unwrap();
//                     debug!("processing token: {:?}", token);
//                     if let Some(prev) = self.prev.clone() {
//                         self.prev = Some(token.clone());
//                         param_tokens.push(prev);
//                     } else {
//                         self.prev = Some(token.clone());
//                     }
//                 }
//             }
//             e => panic!("expected '=' found {:?}", e),
//         }
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct Parameter {
//     pub name: String,
//     // pub pos: Option<ParamPos>,
//     pub value: ParameterValue,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub enum ParameterValue {
//     Atom(String),
//     Array(HashMap<Vec<i64>, String>),
// }

// impl ParameterValue {
//     fn from(pos: Option<ParamPos>, tokens: Vec<Token>) -> Result<Self, ()> {
//         match tokens.len() {
//             0 => panic!("no tokens"),
//             // We have a single value
//             1 => Ok(into_parameter_value_atom(tokens[0].clone())),
//             // We have many values
//             _many => {
//                 println!("tokens: {:?}", tokens);
//                 let vals: Vec<String> = tokens
//                     .into_iter()
//                     .filter(|x| x != &Token::Comma)
//                     .map(|x| match x {
//                         Token::Str(s) => s,
//                         v => panic!("invalid array value: {:?}", v),
//                     })
//                     .collect();
//                 let mut value_map: HashMap<Vec<i64>, String> = HashMap::new();
//                 let indices = pos.map(|p| p.iter()).unwrap_or(ParamPos::default_iter());
//                 for (is, value) in indices.zip(vals.into_iter()) {
//                     value_map.insert(is.into_iter().map(|x| x as i64).collect(), value);
//                 }
//                 Ok(ParameterValue::Array(value_map))
//             }
//         }
//     }
// }

// impl TryFrom<ParameterValue> for bool {
//     type Error = ();

//     fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
//         match pv {
//             ParameterValue::Atom(s) => {
//                 let s_val: NmlBool = s.parse().unwrap();
//                 Ok(s_val.0)
//             }
//             ParameterValue::Array(_) => panic!("expected bool, not array"),
//         }
//     }
// }

// impl TryFrom<ParameterValue> for String {
//     type Error = ();

//     fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
//         match pv {
//             ParameterValue::Atom(s) => {
//                 let s_val: NmlString = s.parse().unwrap();
//                 Ok(s_val.0)
//             }
//             ParameterValue::Array(_) => panic!("expected string, not array"),
//         }
//     }
// }

// impl TryFrom<ParameterValue> for u64 {
//     type Error = ();

//     fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
//         match pv {
//             ParameterValue::Atom(s) => {
//                 let s_val: NmlUint = s.parse().unwrap();
//                 Ok(s_val.0)
//             }
//             ParameterValue::Array(_) => panic!("expected unsigned integer, not array"),
//         }
//     }
// }

// impl TryFrom<ParameterValue> for i64 {
//     type Error = ();

//     fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
//         match pv {
//             ParameterValue::Atom(s) => {
//                 let s_val: NmlInt = s.parse().unwrap();
//                 Ok(s_val.0)
//             }
//             ParameterValue::Array(_) => panic!("expected signed integer, not array"),
//         }
//     }
// }

// impl TryFrom<ParameterValue> for f64 {
//     type Error = ();

//     fn try_from(pv: ParameterValue) -> Result<Self, Self::Error> {
//         match pv {
//             ParameterValue::Atom(s) => {
//                 let s_val: NmlFloat = s.parse().unwrap();
//                 Ok(s_val.0)
//             }
//             ParameterValue::Array(_) => panic!("expected float, not array"),
//         }
//     }
// }

// fn into_parameter_value_atom(token: Token) -> ParameterValue {
//     match token {
//         Token::Str(s) => ParameterValue::Atom(s),
//         _ => panic!("invalid atom value"),
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct ParameterArray {
//     pub pos: ParamPos,
//     pub values: HashMap<Vec<i64>, String>,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct NmlBool(bool);

// impl FromStr for NmlBool {
//     type Err = ();

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         let s = s.trim_matches('.');
//         if s.starts_with("t") {
//             return Ok(NmlBool(true));
//         }
//         if s.starts_with("T") {
//             return Ok(NmlBool(true));
//         }
//         if s.starts_with("f") {
//             return Ok(NmlBool(false));
//         }
//         if s.starts_with("F") {
//             return Ok(NmlBool(false));
//         }
//         Err(())
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct NmlInt(i64);

// impl FromStr for NmlInt {
//     type Err = ();

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         Ok(NmlInt(s.parse().unwrap()))
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct NmlUint(u64);

// impl FromStr for NmlUint {
//     type Err = ();

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         Ok(NmlUint(s.parse().unwrap()))
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct NmlFloat(f64);

// impl FromStr for NmlFloat {
//     type Err = ();

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         Ok(NmlFloat(s.parse().unwrap()))
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct NmlString(String);

// impl FromStr for NmlString {
//     type Err = ();

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         // TODO: fail if it's not wrapped in single quotes (and only once)
//         Ok(NmlString(s.trim_matches('\'').parse().unwrap()))
//     }
// }

// pub fn parameter_name(i: &[u8]) -> IResult<&[u8], String> {
//     let (i, _) = peek(none_of("&=/( \r\n\t0123456789.,"))(i)?;
//     let (i, p_name) = many1(none_of("=/( \r\n\t.,"))(i)?;
//     Ok((i, p_name.into_iter().collect()))
// }

// pub fn parse_nml_name(i: &[u8]) -> IResult<&[u8], String> {
//     let (i, name) = alphanumeric1(i)?;
//     Ok((
//         i,
//         std::str::from_utf8(name)
//             .expect("namelist name is not valid utf8")
//             .to_string(),
//     ))
// }

// pub fn parse_nml_name_any<T>(i: T) -> IResult<T, String>
// where
//     T: nom::InputTakeAtPosition + nom::InputIter,
//     <T as nom::InputTakeAtPosition>::Item: nom::AsChar,
//     <T as nom::InputIter>::Item: nom::AsChar + Copy,
// {
//     use nom::AsChar;
//     let (i, name): (T, T) = alphanumeric1(i)?;
//     let mut v: Vec<char> = Vec::new();
//     for c in name.iter_elements() {
//         v.push(c.as_char())
//     }
//     let name: String = v.into_iter().collect();
//     Ok((i, name))
// }

// pub fn parse_nml_name_str(i: &str) -> IResult<&str, String> {
//     let (i, name) = alphanumeric1(i)?;
//     Ok((i, String::from(name)))
// }

// pub fn quoted_string(i: &[u8]) -> IResult<&[u8], String> {
//     alt((quoted_string_single, quoted_string_double))(i)
// }

// pub fn quoted_string_single(i: &[u8]) -> IResult<&[u8], String> {
//     let (i, _start_quote) = char('\'')(i)?;
//     let (i, s) = many0(none_of("\'"))(i)?;
//     let (i, _end_quote) = char('\'')(i)?;
//     Ok((i, s.iter().collect()))
// }

// pub fn quoted_string_double(i: &[u8]) -> IResult<&[u8], String> {
//     let (i, _start_quote) = char('\"')(i)?;
//     let (i, s) = many0(none_of("\""))(i)?;
//     let (i, _end_quote) = char('\"')(i)?;
//     Ok((i, s.iter().collect()))
// }

// #[derive(Copy, Clone, Debug, PartialEq)]
// pub enum ParamPos {
//     OneDim(Range),
//     TwoDim(Range, Range),
// }

// impl ParamPos {
//     pub fn len(&self) -> usize {
//         use ParamPos::*;
//         match self {
//             OneDim(range) => range.len(),
//             TwoDim(r1, Range::SingleNumber(_)) => r1.len(),
//             TwoDim(Range::SingleNumber(_), r2) => r2.len(),
//             TwoDim(_, _) => panic!(
//                 "In a two-dimensional position parameter, one dimensions must be a single number"
//             ),
//         }
//     }

//     pub fn iter(&self) -> PosIter {
//         match self {
//             ParamPos::OneDim(Range::Numberless) => PosIter {
//                 indices: vec![1],
//                 counting_index: 0,
//                 max: None,
//             },
//             ParamPos::OneDim(Range::SingleNumber(n)) => PosIter {
//                 indices: vec![*n as usize],
//                 counting_index: 0,
//                 max: Some(*n as usize),
//             },
//             ParamPos::OneDim(Range::TwoNumber(n1, n2)) => PosIter {
//                 indices: vec![*n1 as usize],
//                 counting_index: 0,
//                 max: Some(*n2 as usize),
//             },
//             ParamPos::TwoDim(Range::Numberless, Range::Numberless) => {
//                 panic!("single dimensions at a time")
//             }
//             ParamPos::TwoDim(Range::TwoNumber(_, _), Range::Numberless) => {
//                 panic!("single dimensions at a time")
//             }
//             ParamPos::TwoDim(Range::Numberless, Range::TwoNumber(_, _)) => {
//                 panic!("single dimensions at a time")
//             }
//             ParamPos::TwoDim(Range::TwoNumber(_, _), Range::TwoNumber(_, _)) => {
//                 panic!("single dimensions at a time")
//             }
//             ParamPos::TwoDim(Range::SingleNumber(n), Range::Numberless) => PosIter {
//                 indices: vec![*n as usize, 1],
//                 counting_index: 1,
//                 max: None,
//             },
//             ParamPos::TwoDim(Range::Numberless, Range::SingleNumber(n)) => PosIter {
//                 indices: vec![1, *n as usize],
//                 counting_index: 0,
//                 max: None,
//             },
//             ParamPos::TwoDim(Range::SingleNumber(n), Range::TwoNumber(start, end)) => PosIter {
//                 indices: vec![*n as usize, *start as usize],
//                 counting_index: 1,
//                 max: Some(*end as usize),
//             },
//             ParamPos::TwoDim(Range::TwoNumber(start, end), Range::SingleNumber(n)) => PosIter {
//                 indices: vec![*start as usize, *n as usize],
//                 counting_index: 0,
//                 max: Some(*end as usize),
//             },
//             ParamPos::TwoDim(Range::SingleNumber(n1), Range::SingleNumber(n2)) => PosIter {
//                 indices: vec![*n1 as usize, *n2 as usize],
//                 counting_index: 0,
//                 max: Some(*n1 as usize),
//             },
//         }
//     }

//     pub fn default_iter() -> PosIter {
//         PosIter {
//             indices: vec![1],
//             counting_index: 0,
//             max: None,
//         }
//     }
// }

// pub struct PosIter {
//     indices: Vec<usize>,
//     counting_index: usize,
//     max: Option<usize>,
// }

// impl Iterator for PosIter {
//     type Item = Vec<usize>;

//     fn next(&mut self) -> Option<Self::Item> {
//         if let Some(max) = self.max {
//             if *self.indices.get(self.counting_index).unwrap() > max {
//                 return None;
//             }
//         }
//         let item = self.indices.clone();
//         let i = self.indices.get_mut(self.counting_index).unwrap();
//         *i += 1;
//         Some(item)
//     }
// }

// impl TryFrom<Vec<Token>> for ParamPos {
//     type Error = ();

//     fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
//         let sections: Vec<&[Token]> = tokens.split(|x| x == &Token::Comma).collect();
//         let ranges: Vec<Range> = sections
//             .into_iter()
//             .map(|x| x.try_into().unwrap())
//             .collect();
//         match ranges.len() {
//             // We have a single-dimensional value
//             1 => Ok(ParamPos::OneDim(ranges[0])),
//             // We have a two-dimensional value
//             2 => Ok(ParamPos::TwoDim(ranges[0], ranges[1])),
//             // TODO: technically they can have more dimensions
//             _ => panic!("Only 1 or 2 dimensions allowed"),
//         }
//     }
// }

// #[derive(Copy, Clone, Debug, PartialEq)]
// pub enum Range {
//     /// i.e. ":"
//     Numberless,
//     SingleNumber(u64),
//     TwoNumber(u64, u64),
// }

// impl Range {
//     pub fn len(&self) -> usize {
//         match self {
//             Range::Numberless => panic!("not enough information"),
//             Range::SingleNumber(_) => 1_usize,
//             Range::TwoNumber(m, n) => (n - m + 1) as usize,
//         }
//     }
// }

// impl TryFrom<&[Token]> for Range {
//     type Error = ();

//     fn try_from(tokens: &[Token]) -> Result<Self, Self::Error> {
//         match tokens {
//             [Token::Colon] => Ok(Range::Numberless),
//             [Token::Str(s)] => Ok(Range::SingleNumber(s.parse().unwrap())),
//             [Token::Str(s1), Token::Colon, Token::Str(s2)] => {
//                 Ok(Range::TwoNumber(s1.parse().unwrap(), s2.parse().unwrap()))
//             }
//             _ => panic!("too many elements for range"),
//         }
//     }
// }

pub struct NmlParser<R: Read> {
    tokenizer: TokenIter<R>,
    state: ParserState,
    next_namelist: Vec<LocatedToken>,
}

impl<R: Read> NmlParser<R> {
    pub fn new(input: R) -> Self {
        NmlParser {
            tokenizer: TokenIter::new(input),
            state: ParserState::Start,
            next_namelist: Vec::new(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParserState {
    Start,
    InNamelist,
}

impl<R: Read> Iterator for NmlParser<R> {
    type Item = Namelist;

    fn next(&mut self) -> Option<Self::Item> {
        let tokens = loop {
            let token = if let Some(token) = self.tokenizer.next().map(|x| x.unwrap()) {
                token
            } else if !self.next_namelist.is_empty() {
                let tokens = std::mem::take(&mut self.next_namelist);
                break Some(tokens);
            } else {
                break None;
            };
            match self.state {
                ParserState::Start => {
                    if token.token == Token::Ampersand {
                        self.next_namelist.push(token);
                        self.state = ParserState::InNamelist;
                    }
                }
                ParserState::InNamelist => {
                    if token.token == Token::Ampersand {
                        self.state = ParserState::Start;
                        let tokens = std::mem::take(&mut self.next_namelist);
                        self.next_namelist.push(token);
                        break Some(tokens);
                    } else if token.token == Token::RightSlash {
                        self.next_namelist.push(token);
                        self.state = ParserState::Start;
                        let tokens = std::mem::take(&mut self.next_namelist);
                        break Some(tokens);
                    } else {
                        self.next_namelist.push(token);
                    }
                }
            }
        }?;
        parse_namelist(tokens)
    }
}

use std::io::Read;

use namelists::parse_namelist;
use tokenizer::{LocatedToken, Token, TokenIter};

#[cfg(test)]
mod tests {
    use super::*;

    //     #[test]
    //     fn boolean_examples() {
    //         assert_eq!("t".parse(), Ok(NmlBool(true)));
    //         assert_eq!("T".parse(), Ok(NmlBool(true)));
    //         assert_eq!("f".parse(), Ok(NmlBool(false)));
    //         assert_eq!("F".parse(), Ok(NmlBool(false)));
    //         assert_eq!(".FALSE.".parse(), Ok(NmlBool(false)));
    //         assert_eq!(".TRUE.".parse(), Ok(NmlBool(true)));
    //     }

    //     #[test]
    //     fn int_examples() {
    //         assert_eq!("-2".parse(), Ok(NmlInt(-2)));
    //         assert_eq!("60".parse(), Ok(NmlInt(60)));
    //     }

    //     #[test]
    //     fn double_examples() {
    //         assert_eq!("1E13".parse(), Ok(NmlFloat(1e13)));
    //         assert_eq!("2.75E12".parse(), Ok(NmlFloat(2.75e12)));
    //     }

    //     #[test]
    //     fn string_examples() {
    //         assert_eq!("\'hello\'".parse(), Ok(NmlString("hello".to_string())));
    //     }

    //     #[test]
    //     fn float_check() {
    //         assert_eq!("1.1".parse(), Ok(NmlFloat(1.1)));
    //         assert_eq!("123E-02".parse(), Ok(NmlFloat(1.23)));
    //     }

    #[test]
    fn single_nml() {
        let input = "&Head val = 2 /";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<Vec<Token>> = parser
            .map(|nml| {
                let tokens: Vec<Token> = nml.tokens.into_iter().map(|x| x.token).collect();
                tokens
            })
            .collect();
        let expected = vec![vec![
            Token::Ampersand,
            Token::Identifier("Head".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("val".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Equals,
            Token::Whitespace(" ".to_string()),
            Token::Number("2".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::RightSlash,
        ]];
        assert_eq!(nmls, expected);
    }
    #[test]
    fn two_nmls() {
        let input = "&Head val = 2 /\n&DUMP x=2,3,4 /";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<Vec<Token>> = parser
            .map(|nml| {
                let tokens: Vec<Token> = nml.tokens.into_iter().map(|x| x.token).collect();
                tokens
            })
            .collect();
        let expected = vec![
            vec![
                Token::Ampersand,
                Token::Identifier("Head".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Identifier("val".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Equals,
                Token::Whitespace(" ".to_string()),
                Token::Number("2".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::RightSlash,
            ],
            vec![
                Token::Ampersand,
                Token::Identifier("DUMP".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Identifier("x".to_string()),
                Token::Equals,
                Token::Number("2".to_string()),
                Token::Comma,
                Token::Number("3".to_string()),
                Token::Comma,
                Token::Number("4".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::RightSlash,
            ],
        ];
        assert_eq!(nmls, expected);
    }

    //     #[test]
    //     fn nml_iter() {
    //         let f = std::fs::File::open("room_fire.fds").expect("could not open test file");
    //         let parser = NmlParser::new(f);
    //         for nml in parser {
    //             println!("NML: {:?}", nml);
    //         }
    //     }
}
