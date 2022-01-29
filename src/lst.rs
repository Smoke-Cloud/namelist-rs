use std::{collections::VecDeque, str::CharIndices};

#[derive(Clone, Debug, PartialEq)]
pub struct NamelistLst<'input> {
    pub elements: Vec<Element>,
    pub content: &'input str,
}

impl<'input> std::fmt::Display for NamelistLst<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for element in self.elements.iter() {
            let s = &self.content[element.span.start..(element.span.start + element.span.len)];
            write!(f, "{}", s)?;
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
    GroupName,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum ParserState {
    InQuote,
    InIdentifier,
    InWhitespace,
    InComment,
    Start,
}

pub struct NamelistTokenizer<'input> {
    input: &'input str,
    buffer: VecDeque<Element>,
    state: ParserState,
    start: usize,
    char_iter: CharIndices<'input>,
}

impl<'input> NamelistTokenizer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            buffer: VecDeque::new(),
            state: ParserState::Start,
            start: 0,
            char_iter: input.char_indices(),
        }
    }
    pub fn tokenize_nml(self) -> NamelistLst<'input> {
        let content = self.input;
        let elements = self.collect();
        NamelistLst { elements, content }
    }
}
impl<'input> Iterator for NamelistTokenizer<'input> {
    type Item = Element;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(buf) = self.buffer.pop_front() {
            return Some(buf);
        }
        for (i, c) in self.char_iter.by_ref() {
            // for (i, c) in self.input.char_indices() {
            if let ParserState::InQuote = self.state {
                // In this branch we are within a quoted string.
                if c == '\'' {
                    // We're ending a quoted string
                    self.buffer.push_back(Element {
                        token: Token::Str,
                        span: Span {
                            start: self.start,
                            len: i - self.start + 1,
                        },
                    });
                    self.state = ParserState::Start;
                    return self.buffer.pop_front();
                }
                // If we are not ending the quoted string, just keep going.
            } else if c.is_whitespace() {
                match self.state {
                    ParserState::Start => {
                        self.state = ParserState::InWhitespace;
                        self.start = i;
                    }
                    ParserState::InWhitespace => (),
                    ParserState::InComment => {
                        if c == '\n' {
                            // End the comment
                            self.buffer.push_back(Element {
                                token: Token::Comment,
                                span: Span {
                                    start: self.start,
                                    len: i - self.start + 1,
                                },
                            });
                            self.state = ParserState::Start;
                            self.start = i;
                            return self.buffer.pop_front();
                        }
                    }
                    ParserState::InQuote => (),
                    ParserState::InIdentifier => {
                        // We just hit whitespace so finish an identifier.
                        self.buffer.push_back(Element {
                            token: Token::Identifier,
                            span: Span {
                                start: self.start,
                                len: i - self.start,
                            },
                        });
                        self.state = ParserState::InWhitespace;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                }
            } else {
                // In this branch we are not within a quoted string.
                match c {
                    '\'' => {
                        // We have begun a quoted string. Fortran (FDS at least) does not support
                        // escapes, so we can just look for the next single quote.
                        // First we need to end what we were looking at.
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                            ParserState::InComment => {
                                // Don't start a quote if we are in a comment.
                                continue;
                            }
                            ParserState::InQuote => (),
                            ParserState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        // Then we start a quote
                        self.state = ParserState::InQuote;
                        self.start = i;
                    }
                    '!' => {
                        // We have begun a quote, this continues until the end of the line
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Identifier,
                                        span: Span {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            ParserState::InComment => {
                                // Don't start a comment if we are already in a comment.
                                continue;
                            }
                            ParserState::InQuote => unreachable!("cannot be quote"),
                            ParserState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        // Then we start a comment
                        self.state = ParserState::InComment;
                        self.start = i;
                    }
                    '=' => {
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span {
                                            start: self.start,
                                            len: i - self.start,
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
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::Equals,
                            span: Span { start: i, len: 1 },
                        });
                        self.state = ParserState::Start;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                    ',' => {
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span {
                                            start: self.start,
                                            len: i - self.start,
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
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::Comma,
                            span: Span { start: i, len: 1 },
                        });
                        self.state = ParserState::Start;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                    '(' => {
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span {
                                            start: self.start,
                                            len: i - self.start,
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
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::LeftParen,
                            span: Span { start: i, len: 1 },
                        });
                        self.state = ParserState::Start;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                    ')' => {
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span {
                                            start: self.start,
                                            len: i - self.start,
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
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::RightParen,
                            span: Span { start: i, len: 1 },
                        });
                        self.state = ParserState::Start;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                    ':' => {
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span {
                                            start: self.start,
                                            len: i - self.start,
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
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::Colon,
                            span: Span { start: i, len: 1 },
                        });
                        self.state = ParserState::Start;
                        self.start = i + 1;
                        return self.buffer.pop_front();
                    }
                    '/' => {
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span {
                                            start: self.start,
                                            len: i - self.start,
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
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::RightSlash,
                            span: Span { start: i, len: 1 },
                        });
                        self.state = ParserState::Start;
                        self.start = i + 1;
                        return self.buffer.pop_front();
                    }
                    '&' => {
                        match self.state {
                            ParserState::Start => (),
                            ParserState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span {
                                            start: self.start,
                                            len: i - self.start,
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
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::Ampersand,
                            span: Span { start: i, len: 1 },
                        });
                        self.state = ParserState::Start;
                        self.start = i + 1;
                        return self.buffer.pop_front();
                    }
                    _ => match self.state {
                        ParserState::Start => {
                            self.state = ParserState::InIdentifier;
                            self.start = i;
                        }
                        ParserState::InWhitespace => {
                            self.buffer.push_back(Element {
                                token: Token::Whitespace,
                                span: Span {
                                    start: self.start,
                                    len: i - self.start,
                                },
                            });
                            self.state = ParserState::InIdentifier;
                            self.start = i;
                            return self.buffer.pop_front();
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
            // if i
        }
        let len = self.input.len() - self.start;
        match self.state {
            ParserState::Start => (),
            ParserState::InWhitespace => {
                if len > 0 {
                    self.buffer.push_back(Element {
                        token: Token::Whitespace,
                        span: Span {
                            start: self.start,
                            len,
                        },
                    });
                    self.start += len;
                }
            }
            ParserState::InComment => {
                if len > 0 {
                    self.buffer.push_back(Element {
                        token: Token::Comment,
                        span: Span {
                            start: self.start,
                            len,
                        },
                    });
                    self.start += len;
                }
            }
            ParserState::InQuote => {
                panic!("incomplete quote");
            }
            ParserState::InIdentifier => {
                if len > 0 {
                    self.buffer.push_back(Element {
                        token: Token::Identifier,
                        span: Span {
                            start: self.start,
                            len,
                        },
                    });
                    self.start += len;
                }
            }
        }
        self.buffer.pop_front()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn regression_3() {
        let input = "&HEAD CHID='mean_forcing_hole', TITLE='Test HOLE feature for MEAN_FORCING'\n\n&MESH IJK=40,40,20, XB=-20,20,-20,20,0,10 /";
        let result = NamelistTokenizer::new(input).tokenize_nml();
        assert_eq!(input, &result.to_string());
    }

    #[test]
    fn basic_lst() {
        let test_path = "tests/test_input.txt";
        // let f = std::fs::File::open(test_path).expect("could not open test file");
        let input = std::fs::read_to_string(test_path).expect("could not open test file");
        let result = NamelistTokenizer::new(&input).tokenize_nml();
        // for element in result.elements.iter() {
        //     let ss = &result.content[element.span.start..(element.span.start + element.span.len)];
        //     println!("{:?}: {}", element, ss);
        // }
        assert_eq!(input, result.to_string());
    }
}
