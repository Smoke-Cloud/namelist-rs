#[derive(Clone, Debug, PartialEq)]
pub struct NamelistLst {
    pub elements: Vec<Element>,
    pub content: String,
}

impl std::fmt::Display for NamelistLst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for element in self.elements.iter() {
            let ss = &self.content[element.span.start..(element.span.start + element.span.len)];
            write!(f, "{}", ss)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Element {
    pub token: Token,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LeftParen,
    RightParen,
    Equals,
    Colon,
    Comma,
    RightSlash,
    Ampersand,
    Str,
    Whitespace,
    Comment,
    Identifier,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

enum ParserState {
    InQuote,
    InIdentifier,
    InWhitespace,
    InComment,
    Start,
}

fn tokenize_nml(input: String) -> NamelistLst {
    let mut elements: Vec<Element> = Vec::new();
    let mut start: usize = 0;
    let mut parser_state = ParserState::Start;
    for (i, c) in input.char_indices() {
        if let ParserState::InQuote = parser_state {
            // In this branch we are within a quoted string.
            if c == '\'' {
                // We're ending a quoted string
                elements.push(Element {
                    token: Token::Str,
                    span: Span {
                        start,
                        len: i - start + 1,
                    },
                });
                parser_state = ParserState::Start;
            }
            // If we are not ending the quoted string, just keep going.
        } else if c.is_whitespace() {
            match parser_state {
                ParserState::Start => {
                    parser_state = ParserState::InWhitespace;
                    start = i;
                }
                ParserState::InWhitespace | ParserState::InComment => (),
                ParserState::InQuote => (),
                ParserState::InIdentifier => {
                    // We just hit whitespace so finish an identifier.
                    elements.push(Element {
                        token: Token::Identifier,
                        span: Span {
                            start,
                            len: i - start,
                        },
                    });
                    start = i;
                }
            }
        } else {
            // In this branch we are not within a quoted string.
            match c {
                '\'' => {
                    // We have begun a quoted string. Fortran (FDS at least) does not support
                    // escapes, so we can just look for the next single quote.
                    // First we need to end what we were looking at.
                    match parser_state {
                        ParserState::Start => (),
                        ParserState::InWhitespace => {
                            {
                                // We just hit whitespace so finish an identifier.
                                elements.push(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start,
                                        len: i - start,
                                    },
                                });
                            }
                        }
                        ParserState::InComment => {
                            // Don't start a quote if we are in a comment.
                            continue;
                        }
                        ParserState::InQuote => (),
                        ParserState::InIdentifier => {
                            // We just hit whitespace so finish an identifier.
                            elements.push(Element {
                                token: Token::Identifier,
                                span: Span {
                                    start,
                                    len: i - start,
                                },
                            });
                        }
                    }
                    // Then we start a quote
                    parser_state = ParserState::InQuote;
                    start = i;
                }
                '=' => {
                    match parser_state {
                        ParserState::Start => (),
                        ParserState::InWhitespace => {
                            {
                                // We just hit whitespace so finish an identifier.
                                elements.push(Element {
                                    token: Token::Whitespace,
                                    span: Span {
                                        start,
                                        len: i - start,
                                    },
                                });
                            }
                        }
                        ParserState::InComment => {
                            continue;
                        }
                        ParserState::InQuote => {
                            continue;
                        }
                        ParserState::InIdentifier => {
                            // We just hit whitespace so finish an identifier.
                            elements.push(Element {
                                token: Token::Identifier,
                                span: Span {
                                    start,
                                    len: i - start,
                                },
                            });
                        }
                    }
                    elements.push(Element {
                        token: Token::Equals,
                        span: Span { start: i, len: 1 },
                    });
                    parser_state = ParserState::Start;
                    start = i;
                }
                ',' => {
                    match parser_state {
                        ParserState::Start => (),
                        ParserState::InWhitespace => {
                            {
                                // We just hit whitespace so finish an identifier.
                                elements.push(Element {
                                    token: Token::Whitespace,
                                    span: Span {
                                        start,
                                        len: i - start,
                                    },
                                });
                            }
                        }
                        ParserState::InComment => {
                            continue;
                        }
                        ParserState::InQuote => {
                            continue;
                        }
                        ParserState::InIdentifier => {
                            // We just hit whitespace so finish an identifier.
                            elements.push(Element {
                                token: Token::Identifier,
                                span: Span {
                                    start,
                                    len: i - start,
                                },
                            });
                        }
                    }
                    elements.push(Element {
                        token: Token::Comma,
                        span: Span { start: i, len: 1 },
                    });
                    parser_state = ParserState::Start;
                    start = i;
                }
                '(' => {
                    match parser_state {
                        ParserState::Start => (),
                        ParserState::InWhitespace => {
                            {
                                // We just hit whitespace so finish an identifier.
                                elements.push(Element {
                                    token: Token::Whitespace,
                                    span: Span {
                                        start,
                                        len: i - start,
                                    },
                                });
                            }
                        }
                        ParserState::InComment => {
                            continue;
                        }
                        ParserState::InQuote => {
                            continue;
                        }
                        ParserState::InIdentifier => {
                            // We just hit whitespace so finish an identifier.
                            elements.push(Element {
                                token: Token::Identifier,
                                span: Span {
                                    start,
                                    len: i - start,
                                },
                            });
                        }
                    }
                    elements.push(Element {
                        token: Token::LeftParen,
                        span: Span { start: i, len: 1 },
                    });
                    parser_state = ParserState::Start;
                    start = i;
                }
                ')' => {
                    match parser_state {
                        ParserState::Start => (),
                        ParserState::InWhitespace => {
                            {
                                // We just hit whitespace so finish an identifier.
                                elements.push(Element {
                                    token: Token::Whitespace,
                                    span: Span {
                                        start,
                                        len: i - start,
                                    },
                                });
                            }
                        }
                        ParserState::InComment => {
                            continue;
                        }
                        ParserState::InQuote => {
                            continue;
                        }
                        ParserState::InIdentifier => {
                            // We just hit whitespace so finish an identifier.
                            elements.push(Element {
                                token: Token::Identifier,
                                span: Span {
                                    start,
                                    len: i - start,
                                },
                            });
                        }
                    }
                    elements.push(Element {
                        token: Token::RightParen,
                        span: Span { start: i, len: 1 },
                    });
                    parser_state = ParserState::Start;
                    start = i;
                }
                ':' => {
                    match parser_state {
                        ParserState::Start => (),
                        ParserState::InWhitespace => {
                            {
                                // We just hit whitespace so finish an identifier.
                                elements.push(Element {
                                    token: Token::Whitespace,
                                    span: Span {
                                        start,
                                        len: i - start,
                                    },
                                });
                            }
                        }
                        ParserState::InComment => {
                            continue;
                        }
                        ParserState::InQuote => {
                            continue;
                        }
                        ParserState::InIdentifier => {
                            // We just hit whitespace so finish an identifier.
                            elements.push(Element {
                                token: Token::Identifier,
                                span: Span {
                                    start,
                                    len: i - start,
                                },
                            });
                        }
                    }
                    elements.push(Element {
                        token: Token::Colon,
                        span: Span { start: i, len: 1 },
                    });
                    parser_state = ParserState::Start;
                    start = i + 1;
                }
                '/' => {
                    match parser_state {
                        ParserState::Start => (),
                        ParserState::InWhitespace => {
                            {
                                // We just hit whitespace so finish an identifier.
                                elements.push(Element {
                                    token: Token::Whitespace,
                                    span: Span {
                                        start,
                                        len: i - start,
                                    },
                                });
                            }
                        }
                        ParserState::InComment => {
                            continue;
                        }
                        ParserState::InQuote => {
                            continue;
                        }
                        ParserState::InIdentifier => {
                            // We just hit whitespace so finish an identifier.
                            elements.push(Element {
                                token: Token::Identifier,
                                span: Span {
                                    start,
                                    len: i - start,
                                },
                            });
                        }
                    }
                    elements.push(Element {
                        token: Token::RightSlash,
                        span: Span { start: i, len: 1 },
                    });
                    parser_state = ParserState::Start;
                    start = i + 1;
                }
                '&' => {
                    match parser_state {
                        ParserState::Start => (),
                        ParserState::InWhitespace => {
                            {
                                // We just hit whitespace so finish an identifier.
                                elements.push(Element {
                                    token: Token::Whitespace,
                                    span: Span {
                                        start,
                                        len: i - start,
                                    },
                                });
                            }
                        }
                        ParserState::InComment => {
                            continue;
                        }
                        ParserState::InQuote => {
                            continue;
                        }
                        ParserState::InIdentifier => {
                            // We just hit whitespace so finish an identifier.
                            elements.push(Element {
                                token: Token::Identifier,
                                span: Span {
                                    start,
                                    len: i - start,
                                },
                            });
                        }
                    }
                    elements.push(Element {
                        token: Token::Ampersand,
                        span: Span { start: i, len: 1 },
                    });
                    parser_state = ParserState::Start;
                    start = i + 1;
                }
                _ => match parser_state {
                    ParserState::Start => {
                        parser_state = ParserState::InIdentifier;
                        start = i;
                    }
                    ParserState::InWhitespace => {
                        elements.push(Element {
                            token: Token::Whitespace,
                            span: Span {
                                start,
                                len: i - start,
                            },
                        });
                        parser_state = ParserState::InIdentifier;
                        start = i;
                    }
                    ParserState::InComment => {
                        continue;
                    }
                    ParserState::InQuote => {
                        continue;
                    }
                    ParserState::InIdentifier => (),
                },
            }
        }
    }
    NamelistLst {
        elements,
        content: input,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn regression_3() {
        let input = "&HEAD CHID='mean_forcing_hole', TITLE='Test HOLE feature for MEAN_FORCING'\n\n&MESH IJK=40,40,20, XB=-20,20,-20,20,0,10 /";
        let result = tokenize_nml(input.to_string());
        for element in result.elements.iter() {
            let ss = &result.content[element.span.start..(element.span.start + element.span.len)];
            println!("{:?}: {}", element, ss);
        }
        assert_eq!(input, &result.to_string());
        panic!("{}", result);
    }
}
