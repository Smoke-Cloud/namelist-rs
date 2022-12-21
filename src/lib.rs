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

impl Display for Namelist {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.tokens {
            write!(f, "{token}")?;
        }
        Ok(())
    }
}

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
                        let tokens = std::mem::take(&mut self.next_namelist);
                        self.next_namelist.push(token);
                        self.state = ParserState::InNamelist;
                        if !tokens.is_empty() {
                            break Some(tokens);
                        }
                    } else {
                        self.next_namelist.push(token);
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

use std::{fmt::Display, io::Read};

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
            vec![Token::Whitespace("\n".to_string())],
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
}
