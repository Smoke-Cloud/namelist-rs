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
    Number(String),
}

pub enum TokenizerState {
    Start,
    InQuote { start: usize, content: String },
    InIdentifier { start: usize, content: String },
    InNumber { start: usize, content: String },
    InWhitespace { start: usize, content: String },
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
                    state = TokenizerState::InWhitespace { start, content };
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
                            if c.is_alphabetic() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InIdentifier { start, content };
                            } else if c.is_whitespace() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InWhitespace { start, content };
                            } else if c.is_digit(10) || c == '-' {
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InNumber { start: i, content };
                            } else {
                                panic!("{} is an invalid character", c)
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
            TokenizerState::InWhitespace { start, mut content } => {
                if c.is_whitespace() {
                    content.push(c);
                    state = TokenizerState::InWhitespace { start, content };
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
                            if c.is_alphabetic() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InIdentifier { start, content };
                            } else if c.is_whitespace() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InWhitespace { start, content };
                            } else if c.is_digit(10)
                                || c == '.'
                                || c == 'e'
                                || c == 'E'
                                || c == '-'
                                || c == '+'
                            {
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InNumber { start: i, content };
                            } else {
                                panic!("{} is an invalid character", c)
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
                    state = TokenizerState::InWhitespace { start, content };
                } else if c.is_alphabetic() || c == '_' {
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
                        _ => {
                            if c.is_digit(10) {
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InNumber { start: i, content };
                            } else {
                                panic!("{} is an invalid character", c)
                            }
                        }
                    }
                }
            }
            TokenizerState::InNumber { start, mut content } => {
                if c.is_digit(10) || c == '.' || c == 'e' || c == '-' {
                    content.push(c);
                    state = TokenizerState::InNumber { start, content };
                } else {
                    let len = content.len();
                    let token = Token::Number(content);
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
                            if c.is_alphabetic() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InIdentifier { start, content };
                            } else if c.is_whitespace() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                state = TokenizerState::InWhitespace { start, content };
                            } else {
                                panic!("{} is an invalid character", c)
                            }
                        }
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
        TokenizerState::InWhitespace { start, content } => {
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
        TokenizerState::InNumber { start, content } => {
            let len = content.len();
            let token = Token::Number(content);
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
                    token: Token::Number("2".to_string()),
                }
            ],
            tokens
        );
    }

    #[test]
    fn trivial_tokens2() {
        let s = "abc= 2";
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
                    token: Token::Whitespace(" ".to_string()),
                },
                LocatedToken {
                    span: Span { lo: 5, len: 1 },
                    token: Token::Number("2".to_string()),
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
                token: Token::Number("2".to_string()),
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
                token: Token::Number("2".to_string()),
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
        assert_eq!(expected, tokens);
    }

    #[test]
    fn simple_tokens2() {
        assert_eq!(
            tokenize_nml("TEMPERATURES(1:2)=273.15, 274"),
            vec![
                LocatedToken {
                    span: Span { lo: 0, len: 12 },
                    token: Token::Identifier("TEMPERATURES".to_string())
                },
                LocatedToken {
                    span: Span { lo: 12, len: 1 },
                    token: Token::LeftBracket
                },
                LocatedToken {
                    span: Span { lo: 13, len: 1 },
                    token: Token::Number("1".to_string())
                },
                LocatedToken {
                    span: Span { lo: 14, len: 1 },
                    token: Token::Colon
                },
                LocatedToken {
                    span: Span { lo: 15, len: 1 },
                    token: Token::Number("2".to_string())
                },
                LocatedToken {
                    span: Span { lo: 16, len: 1 },
                    token: Token::RightBracket
                },
                LocatedToken {
                    span: Span { lo: 17, len: 1 },
                    token: Token::Equals
                },
                LocatedToken {
                    span: Span { lo: 18, len: 6 },
                    token: Token::Number("273.15".to_string())
                },
                LocatedToken {
                    span: Span { lo: 24, len: 1 },
                    token: Token::Comma
                },
                LocatedToken {
                    span: Span { lo: 25, len: 1 },
                    token: Token::Whitespace(" ".to_string())
                },
                LocatedToken {
                    span: Span { lo: 26, len: 3 },
                    token: Token::Number("274".to_string())
                }
            ]
        );
    }
}
