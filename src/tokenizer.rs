#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    lo: usize,
    len: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocatedToken {
    span: Span,
    token: Token,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token {
    LeftBracket,
    RightBracket,
    Equals,
    Colon,
    Comma,
    RightSlash,
    /// Some variable string that forms a token. Currently this could also
    /// include numbers.
    QuotedStr(String),
    Whitespace(String),
    Identifier(String),
}

pub enum TokenizerState {
    Start,
    InQuote { start: usize, content: String },
    InIdentifier { start: usize, content: String },
    Whitespace { start: usize, content: String },
}

pub fn tokenize_nml(input: &str) -> Vec<LocatedToken> {
    let mut state = TokenizerState::Start;
    let mut tokens: Vec<LocatedToken> = Vec::new();
    // let mut start: usize = 0;
    // We have begun a quoted string. Fortran (FDS at least) does not support
    // escapes, so we can just look for the next single quote.
    for (i, c) in input.char_indices() {
        match state {
            TokenizerState::Start => {
                if c.is_whitespace() {
                    let start = i;
                    let mut content = String::new();
                    content.push(c);
                    state = TokenizerState::Whitespace { start, content };
                } else {
                    match c {
                        '\'' => {
                            let start = i;
                            let mut content = String::new();
                            content.push(c);
                            state = TokenizerState::InQuote { start, content };
                        }
                        '=' => {
                            let token = Token::Equals;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        '(' => {
                            let token = Token::LeftBracket;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ')' => {
                            let token = Token::RightBracket;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ':' => {
                            let token = Token::Colon;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ',' => {
                            let token = Token::Comma;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        _ => {
                            if c.is_alphanumeric() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InIdentifier { start, content };
                            } else if c.is_alphanumeric() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::Whitespace { start, content };
                            } else {
                                panic!("{} is an invalid character", c);
                            }
                        }
                    }
                }
            }
            TokenizerState::InQuote { start, mut content } => match c {
                '\'' => {
                    content.push(c);
                    let len = content.len();
                    let token = Token::QuotedStr(content);
                    let span = Span { lo: start, len };
                    tokens.push(LocatedToken { span, token });
                    state = TokenizerState::Start;
                }
                _ => {
                    content.push(c);
                    state = TokenizerState::InQuote { start, content };
                }
            },
            TokenizerState::Whitespace { start, mut content } => {
                if c.is_whitespace() {
                    content.push(c);
                    state = TokenizerState::Whitespace { start, content };
                } else {
                    let len = content.len();
                    let token = Token::Whitespace(content);
                    let span = Span { lo: start, len };
                    tokens.push(LocatedToken { span, token });
                    match c {
                        '\'' => {
                            let start = i;
                            let mut content = String::new();
                            content.push(c);
                            state = TokenizerState::InQuote { start, content };
                        }
                        '=' => {
                            let token = Token::Equals;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        '(' => {
                            let token = Token::LeftBracket;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ')' => {
                            let token = Token::RightBracket;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ':' => {
                            let token = Token::Colon;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ',' => {
                            let token = Token::Comma;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        _ => {
                            if c.is_alphanumeric() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InIdentifier { start, content };
                            } else if c.is_alphanumeric() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::Whitespace { start, content };
                            } else {
                                panic!("{} is an invalid character", c);
                            }
                        }
                    }
                }
            }
            TokenizerState::InIdentifier { start, mut content } => {
                if c.is_whitespace() {
                    let len = content.len();
                    let token = Token::Identifier(content);
                    let span = Span { lo: start, len };
                    tokens.push(LocatedToken { span, token });
                    let start = i;
                    let mut content = String::new();
                    content.push(c);
                    state = TokenizerState::Whitespace { start, content };
                } else if c.is_alphanumeric() || c == '_' {
                    content.push(c);
                    state = TokenizerState::InIdentifier { start, content };
                } else {
                    let len = content.len();
                    let token = Token::Identifier(content);
                    let span = Span { lo: start, len };
                    tokens.push(LocatedToken { span, token });
                    match c {
                        '\'' => {
                            let start = i;
                            let mut content = String::new();
                            content.push(c);
                            state = TokenizerState::InQuote { start, content };
                        }
                        '=' => {
                            let token = Token::Equals;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        '(' => {
                            let token = Token::LeftBracket;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ')' => {
                            let token = Token::RightBracket;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ':' => {
                            let token = Token::Colon;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        ',' => {
                            let token = Token::Comma;
                            let span = Span { lo: i, len: 1 };
                            tokens.push(LocatedToken { span, token });
                            state = TokenizerState::Start;
                        }
                        _ => panic!("{} is an invalid character", c),
                    }
                }
            }
        }
    }
    match state {
        TokenizerState::Start => {}
        TokenizerState::InQuote { .. } => {
            panic!("Unclosed quoted string")
        }
        TokenizerState::Whitespace { start, content } => {
            let len = content.len();
            let token = Token::Whitespace(content);
            let span = Span { lo: start, len };
            tokens.push(LocatedToken { span, token });
        }
        TokenizerState::InIdentifier { start, content } => {
            let len = content.len();
            let token = Token::Identifier(content);
            let span = Span { lo: start, len };
            tokens.push(LocatedToken { span, token });
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trivial_tokens1() {
        let s = "abc=2";
        let tokens = tokenize_nml(s);
        assert_eq!(
            vec![
                LocatedToken {
                    span: Span { lo: 0, len: 3 },
                    token: Token::Identifier("abc".to_string()),
                },
                LocatedToken {
                    span: Span { lo: 3, len: 1 },
                    token: Token::Equals,
                },
                LocatedToken {
                    span: Span { lo: 4, len: 1 },
                    token: Token::Identifier("2".to_string()),
                }
            ],
            tokens
        );
    }

    #[test]
    fn simple_tokens1() {
        let s = "abc=2,'ad c' (2,:)";
        let tokens = tokenize_nml(s);
        let expected = vec![
            LocatedToken {
                span: Span { lo: 0, len: 3 },
                token: Token::Identifier("abc".to_string()),
            },
            LocatedToken {
                span: Span { lo: 3, len: 1 },
                token: Token::Equals,
            },
            LocatedToken {
                span: Span { lo: 4, len: 1 },
                token: Token::Identifier("2".to_string()),
            },
            LocatedToken {
                span: Span { lo: 5, len: 1 },
                token: Token::Comma,
            },
            LocatedToken {
                span: Span { lo: 6, len: 6 },
                token: Token::QuotedStr("'ad c'".to_string()),
            },
            LocatedToken {
                span: Span { lo: 12, len: 1 },
                token: Token::Whitespace(" ".to_string()),
            },
            LocatedToken {
                span: Span { lo: 13, len: 1 },
                token: Token::LeftBracket,
            },
            LocatedToken {
                span: Span { lo: 14, len: 1 },
                token: Token::Identifier("2".to_string()),
            },
            LocatedToken {
                span: Span { lo: 15, len: 1 },
                token: Token::Comma,
            },
            LocatedToken {
                span: Span { lo: 16, len: 1 },
                token: Token::Colon,
            },
            LocatedToken {
                span: Span { lo: 17, len: 1 },
                token: Token::RightBracket,
            },
        ];
        // assert_eq!(expected, tokens);
        // assert_eq!(expected.len(), tokens.len());
        for (expected, found) in expected.into_iter().zip(tokens.into_iter()) {
            assert_eq!(expected, found);
        }
    }

    // #[test]
    // fn simple_tokens2() {
    //     assert_eq!(
    //         tokenize_nml("TEMPERATURES(1:2)=273, 274"),
    //         vec![
    //             "TEMPERATURES",
    //             "(",
    //             "1",
    //             ":",
    //             "2",
    //             ")",
    //             "=",
    //             "273",
    //             ",",
    //             "274",
    //         ]
    //     );
    // }
}
