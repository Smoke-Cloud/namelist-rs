pub mod namelists;
pub mod tokenizer;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamelistFile {
    pub namelists: Vec<Namelist>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Namelist {
    Actual { tokens: Vec<LocatedToken> },
    Other { tokens: Vec<LocatedToken> },
}

impl Namelist {
    pub fn tokens(&self) -> &[LocatedToken] {
        match self {
            Self::Actual { tokens } => tokens,
            Self::Other { tokens } => tokens,
        }
    }
    pub fn into_tokens(self) -> Vec<LocatedToken> {
        match self {
            Self::Actual { tokens } => tokens,
            Self::Other { tokens } => tokens,
        }
    }
    pub fn append_token(&mut self, token: Token) {
        let located = LocatedToken {
            span: Span { lo: 0, len: 0 },
            token,
        };
        let tokens = match self {
            Self::Actual { tokens } => tokens,
            Self::Other { tokens } => tokens,
        };
        if tokens.last().map(|l| &l.token) == Some(&Token::RightSlash) {
            tokens.insert(tokens.len() - 1, located);
        } else {
            tokens.push(located);
        }
    }
}

impl Display for Namelist {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in self.tokens() {
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
use tokenizer::{LocatedToken, Span, Token, TokenIter};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Span;

    #[test]
    fn single_nml() {
        let input = "&Head val = 2 /";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<Namelist> = parser.collect();
        let expected = vec![Namelist::Actual {
            tokens: vec![
                LocatedToken {
                    token: Token::Ampersand,
                    span: Span { lo: 0, len: 1 },
                },
                LocatedToken {
                    token: Token::Identifier("Head".to_string()),
                    span: Span { lo: 1, len: 4 },
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Span { lo: 5, len: 1 },
                },
                LocatedToken {
                    token: Token::Identifier("val".to_string()),
                    span: Span { lo: 6, len: 3 },
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Span { lo: 9, len: 1 },
                },
                LocatedToken {
                    token: Token::Equals,
                    span: Span { lo: 10, len: 1 },
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Span { lo: 11, len: 1 },
                },
                LocatedToken {
                    token: Token::Number("2".to_string()),
                    span: Span { lo: 12, len: 1 },
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Span { lo: 13, len: 1 },
                },
                LocatedToken {
                    token: Token::RightSlash,
                    span: Span { lo: 14, len: 1 },
                },
            ],
        }];
        assert_eq!(nmls, expected);
    }
    #[test]
    fn single_nml_append() {
        let input = "&Head val = 2 /";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        let mut nmls: Vec<Namelist> = parser.collect();
        if let Some(nml) = nmls.last_mut() {
            nml.append_token(Token::Identifier("hello".to_string()))
        }
        let expected = vec![Namelist::Actual {
            tokens: vec![
                LocatedToken {
                    token: Token::Ampersand,
                    span: Span { lo: 0, len: 1 },
                },
                LocatedToken {
                    token: Token::Identifier("Head".to_string()),
                    span: Span { lo: 1, len: 4 },
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Span { lo: 5, len: 1 },
                },
                LocatedToken {
                    token: Token::Identifier("val".to_string()),
                    span: Span { lo: 6, len: 3 },
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Span { lo: 9, len: 1 },
                },
                LocatedToken {
                    token: Token::Equals,
                    span: Span { lo: 10, len: 1 },
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Span { lo: 11, len: 1 },
                },
                LocatedToken {
                    token: Token::Number("2".to_string()),
                    span: Span { lo: 12, len: 1 },
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Span { lo: 13, len: 1 },
                },
                LocatedToken {
                    token: Token::Identifier("hello".to_string()),
                    span: Span { lo: 0, len: 0 },
                },
                LocatedToken {
                    token: Token::RightSlash,
                    span: Span { lo: 14, len: 1 },
                },
            ],
        }];
        assert_eq!(nmls, expected);
    }

    #[test]
    fn two_nmls() {
        let input = "&Head val = 2 /\n&DUMP x=2,3,4 /";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        let nmls: Vec<Vec<Token>> = parser
            .map(|nml| {
                let tokens: Vec<Token> = nml.into_tokens().into_iter().map(|x| x.token).collect();
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
            vec![Token::Comment("\n".to_string())],
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
