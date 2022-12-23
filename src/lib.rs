pub mod namelists;
pub mod tokenizer;
use namelists::parse_namelist;
use std::{collections::HashMap, fmt::Display, io::Read, slice::Iter};
use tokenizer::{LocatedToken, NmlParseError, Span, Token, TokenIter, TokenizerError};

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
        let located = LocatedToken { span: None, token };
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
    type Item = Result<Namelist, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let tokens = loop {
            let token = match self.tokenizer.next() {
                Some(Ok(token)) => token,
                Some(Err(err)) => {
                    return Some(Err(err));
                }
                None => {
                    if !self.next_namelist.is_empty() {
                        let tokens = std::mem::take(&mut self.next_namelist);
                        break Some(tokens);
                    } else {
                        break None;
                    }
                }
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
        parse_namelist(tokens).map(Ok)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedNamelist {
    pub group: String,
    pub span: Option<Span>,
    pub parameters: HashMap<String, ParameterValues>,
}

impl ParsedNamelist {
    pub fn from_namelist(nml: &Namelist) -> Result<ParsedNamelist, NmlParseError> {
        let mut tokens = nml.tokens().iter();
        let first_token = tokens.next().ok_or(NmlParseError::NoTokens)?;
        if first_token.token != Token::Ampersand {
            return Err(NmlParseError::NoAmpersand(first_token.span));
        }

        let (group, group_span) = {
            let gn = tokens.next().ok_or(NmlParseError::NoTokens)?;
            if let Token::Identifier(s) = &gn.token {
                (s.to_string(), gn.span())
            } else {
                return Err(NmlParseError::InvalidGroupName(gn.span));
            }
        };
        let mut parameters: HashMap<String, ParameterValues> = Default::default();
        let mut token_buf: Vec<LocatedToken> = vec![];
        while let Some(pn) = token_buf
            .pop()
            .or_else(|| next_non_ws(&mut tokens).cloned())
        {
            // Take parameter name
            let parameter_name = pn.clone();
            if parameter_name.token == Token::RightSlash {
                break;
            }
            if let Token::Identifier(name) = &parameter_name.token {
                {
                    let b = token_buf.pop();
                    let token = b.as_ref().or_else(|| next_non_ws(&mut tokens));
                    if let Some(Token::Equals) = token.map(|lt| &lt.token) {
                    } else {
                        return Err(NmlParseError::NoEquals(token.and_then(|t| t.span)));
                    };
                }
                let mut value_tokens: Vec<LocatedToken> = vec![];
                while let Some(token) = token_buf
                    .pop()
                    .as_ref()
                    .or_else(|| next_non_ws(&mut tokens))
                {
                    if token.token == Token::RightSlash {
                        break;
                    }
                    if token.token == Token::Equals {
                        token_buf.push(token.clone());
                        if let Some(t) = value_tokens.pop() {
                            token_buf.push(t);
                        }
                        break;
                    }
                    value_tokens.push(token.clone());
                }
                parameters.insert(
                    name.to_string(),
                    ParameterValues {
                        span: pn.span,
                        dimensions: vec![],
                        values: value_tokens,
                    },
                );
                // loop until we hit equals or right slash
                continue;
            } else {
                return Err(NmlParseError::InvalidParameterName(parameter_name.span));
            }
        }
        Ok(ParsedNamelist {
            group,
            span: group_span,
            parameters,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParameterValues {
    pub span: Option<Span>,
    pub dimensions: Vec<LocatedToken>,
    pub values: Vec<LocatedToken>,
}

fn next_non_ws<'a>(tokens: &'a mut Iter<LocatedToken>) -> Option<&'a LocatedToken> {
    loop {
        let token = tokens.next()?;
        match token.token {
            Token::Whitespace(_) | Token::Comma => {
                continue;
            }
            _ => return Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Span;

    #[test]
    fn single_nml() {
        let input = "&Head val = 2 /";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        let nmls = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        let expected = vec![Namelist::Actual {
            tokens: vec![
                LocatedToken {
                    token: Token::Ampersand,
                    span: Some(Span {
                        lo: 0,
                        len: 1,
                        column: 0,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::Identifier("Head".to_string()),
                    span: Some(Span {
                        lo: 1,
                        len: 4,
                        column: 1,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Some(Span {
                        lo: 5,
                        len: 1,
                        column: 5,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::Identifier("val".to_string()),
                    span: Some(Span {
                        lo: 6,
                        len: 3,
                        column: 6,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Some(Span {
                        lo: 9,
                        len: 1,
                        column: 9,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::Equals,
                    span: Some(Span {
                        lo: 10,
                        len: 1,
                        column: 10,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Some(Span {
                        lo: 11,
                        len: 1,
                        column: 11,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::Number("2".to_string()),
                    span: Some(Span {
                        lo: 12,
                        len: 1,
                        column: 12,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::Whitespace(" ".to_string()),
                    span: Some(Span {
                        lo: 13,
                        len: 1,
                        column: 13,
                        line: 0,
                    }),
                },
                LocatedToken {
                    token: Token::RightSlash,
                    span: Some(Span {
                        lo: 14,
                        len: 1,
                        column: 14,
                        line: 0,
                    }),
                },
            ],
        }];
        assert_eq!(nmls, expected);
    }
    #[test]
    fn single_nml_append() {
        let input = "&Head val = 2 /";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        let mut nmls = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        if let Some(nml) = nmls.last_mut() {
            nml.append_token(Token::Identifier("hello".to_string()))
        }
        let nmls: Vec<Vec<Token>> = nmls
            .into_iter()
            .map(|x| x.tokens().iter().map(|x| x.token.clone()).collect())
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
            Token::Identifier("hello".to_string()),
            Token::RightSlash,
        ]];
        assert_eq!(nmls, expected);
    }

    #[test]
    fn location1() {
        let input = "&Head val(1:3)= 1,2,3 /";
        let parser = NmlParser::new(std::io::Cursor::new(input));
        let mut nmls = parser
            .collect::<Result<Vec<Namelist>, _>>()
            .expect("test parse failed");
        if let Some(nml) = nmls.last_mut() {
            nml.append_token(Token::Identifier("hello".to_string()))
        }
        let nmls: Vec<Vec<Token>> = nmls
            .into_iter()
            .map(|x| x.tokens().iter().map(|x| x.token.clone()).collect())
            .collect();
        let expected = vec![vec![
            Token::Ampersand,
            Token::Identifier("Head".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("val".to_string()),
            Token::LeftBracket,
            Token::Number("1".to_string()),
            Token::Colon,
            Token::Number("3".to_string()),
            Token::RightBracket,
            Token::Equals,
            Token::Whitespace(" ".to_string()),
            Token::Number("1".to_string()),
            Token::Comma,
            Token::Number("2".to_string()),
            Token::Comma,
            Token::Number("3".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("hello".to_string()),
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
                let tokens: Result<Vec<Token>, _> =
                    nml.map(|nml| nml.into_tokens().into_iter().map(|x| x.token).collect());
                tokens
            })
            .collect::<Result<Vec<Vec<Token>>, _>>()
            .expect("test parse failed");
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
