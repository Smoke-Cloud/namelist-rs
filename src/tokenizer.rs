#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    lo: usize,
    hi: usize,
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
                        _ => {
                            let start = i;
                            let mut content = String::new();
                            content.push(c);
                            state = TokenizerState::InIdentifier { start, content };
                        }
                    }
                }
            }
            TokenizerState::InQuote { start, mut content } => {
                match c {
                    '\'' => {
                        content.push(c);
                        let token = Token::QuotedStr(content);
                        let span = Span { lo: start, hi: i };
                        tokens.push(LocatedToken { span, token });
                        state = TokenizerState::Start;
                    }
                    // '=' | ',' | '(' | ')' | ':' | '/' => {
                    //     let previous = &input[start..i];
                    //     // if !previous.is_empty() {
                    //     //     tokens.push(previous);
                    //     // }
                    //     // tokens.push(&input[i..=i]);
                    //     start = i + 1;
                    // }
                    _ => {
                        content.push(c);
                        state = TokenizerState::InQuote { start, content };
                    }
                }
            }
            TokenizerState::Whitespace { start, mut content } => {
                if c.is_whitespace() {
                    content.push(c);
                    state = TokenizerState::Whitespace { start, content };
                } else {
                    match c {
                        '\'' => {
                            let mut content = String::new();
                            content.push(c);
                            state = TokenizerState::InQuote { start, content };
                        }
                        _ => {
                            let mut content = String::new();
                            content.push(c);
                            state = TokenizerState::InIdentifier { start, content };
                        }
                    }
                }
            }
            TokenizerState::InIdentifier { start, mut content } => {
                if !c.is_whitespace() {
                    content.push(c);
                    state = TokenizerState::InIdentifier { start, content };
                } else {
                    let token = Token::Identifier(content);
                    let span = Span { lo: start, hi: i };
                    tokens.push(LocatedToken { span, token });
                    let start = i;
                    let mut content = String::new();
                    content.push(c);
                    state = TokenizerState::Whitespace { start, content };
                }
            }
        }
    }
    let i = if !input.is_empty() { input.len() - 1 } else { 0 };
    match state {
        TokenizerState::Start => {}
        TokenizerState::InQuote { .. } => {
            panic!("Unclosed quoted string")
        }
        TokenizerState::Whitespace { start, content } => {
            let token = Token::Whitespace(content);
            let span = Span { lo: start, hi: i };
            tokens.push(LocatedToken { span, token });
        }
        TokenizerState::InIdentifier { start, content } => {
            let token = Token::Identifier(content);
            let span = Span { lo: start, hi: i };
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
            vec![LocatedToken {
                span: Span { lo: 0, hi: 4 },
                token: Token::Identifier("abc=2".to_string()),
            }],
            tokens
        );
    }

    // #[test]
    // fn simple_tokens1() {
    //     let s = "abc=2,'ad c' (2,:)";
    //     let tokens = tokenize_nml(s);
    //     assert_eq!(
    //         vec!["abc", "=", "2", ",", "'ad c'", "(", "2", ",", ":", ")"],
    //         tokens
    //     );
    // }

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
