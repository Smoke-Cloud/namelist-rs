use std::{collections::VecDeque, str::CharIndices, vec};

#[derive(Clone, Debug, PartialEq)]
pub struct NamelistFile<'input> {
    pub elements: Vec<NamelistElement>,
    pub content: &'input str,
}

impl<'input> std::fmt::Display for NamelistFile<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for nml in self.elements.iter() {
            let elems = match nml {
                NamelistElement::Namelist(nml_elem) => &nml_elem.tokens,
                NamelistElement::Other(elems) => elems,
            };
            for element in elems.iter() {
                let s = element.as_str(self.content);
                write!(f, "{}", s)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NamelistElement {
    Namelist(Namelist),
    Other(Vec<Element>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Namelist {
    pub name: String,
    pub tokens: Vec<Element>,
}

impl Namelist {
    pub fn remove_parameter(&mut self, name: &str) {
        todo!("remove_parameter")
    }
    pub fn add_parameter(&mut self, name: &str, value: &str) {
        todo!("remove_parameter")
    }
    pub fn replace_parameter(&mut self, name: &str, value: &str) {
        self.remove_parameter(name);
        self.add_parameter(name, value);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamelistLst<'input> {
    pub elements: Vec<Element>,
    pub content: &'input str,
}

impl<'input> std::fmt::Display for NamelistLst<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for element in self.elements.iter() {
            write!(f, "{}", element.as_str(self.content))?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Element {
    pub token: Token,
    pub span: Span,
}

impl<'a> Element {
    pub fn as_str(&'a self, content: &'a str) -> &'a str {
        self.span.as_str(content)
    }
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
pub enum Span {
    Backed { start: usize, len: usize },
    Owned(String),
}

impl<'a> Span {
    pub fn as_str(&'a self, content: &'a str) -> &'a str {
        match self {
            Span::Backed { start, len } => &content[*start..(*start + *len)],
            Span::Owned(s) => s.as_str(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenizerState {
    InQuote,
    InIdentifier,
    InWhitespace,
    InComment,
    Start,
}

pub struct NamelistParser<'input> {
    input: &'input str,
    buffer: Option<Element>,
    token_iter: NamelistTokenizer<'input>,
}

impl<'input> NamelistParser<'input> {
    pub fn new(input: &'input str, token_iter: NamelistTokenizer<'input>) -> Self {
        Self {
            input,
            buffer: None,
            token_iter,
        }
    }
    pub fn into_nml_file(self) -> NamelistFile<'input> {
        let content = self.input;
        let elements = self.collect();
        NamelistFile { elements, content }
    }
}

impl<'input> Iterator for NamelistParser<'input> {
    type Item = NamelistElement;
    fn next(&mut self) -> Option<Self::Item> {
        let element = self.buffer.take().or_else(|| self.token_iter.next());
        if let Some(element) = element {
            if element.token == Token::Ampersand {
                if let Some(next_element) = self.token_iter.next() {
                    let mut namelist = Namelist {
                        name: next_element.as_str(self.input).to_string(),
                        tokens: vec![element, next_element],
                    };
                    for element in self.token_iter.by_ref() {
                        let end = element.token == Token::RightSlash;
                        namelist.tokens.push(element);
                        if end {
                            return Some(NamelistElement::Namelist(namelist));
                        }
                    }
                    Some(NamelistElement::Namelist(namelist))
                } else {
                    panic!("no namelist name");
                }
            } else {
                let mut others = vec![element];
                for element in self.token_iter.by_ref() {
                    if element.token == Token::Ampersand {
                        self.buffer = Some(element);
                        return Some(NamelistElement::Other(others));
                    } else {
                        others.push(element);
                    }
                }
                Some(NamelistElement::Other(others))
            }
        } else {
            None
        }
    }
}

pub struct NamelistTokenizer<'input> {
    input: &'input str,
    buffer: VecDeque<Element>,
    state: TokenizerState,
    start: usize,
    char_iter: CharIndices<'input>,
}

impl<'input> NamelistTokenizer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            buffer: VecDeque::new(),
            state: TokenizerState::Start,
            start: 0,
            char_iter: input.char_indices(),
        }
    }
    pub fn into_parser(self) -> NamelistParser<'input> {
        NamelistParser::new(self.input, self)
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
            if let TokenizerState::InQuote = self.state {
                // In this branch we are within a quoted string.
                if c == '\'' {
                    // We're ending a quoted string
                    self.buffer.push_back(Element {
                        token: Token::Str,
                        span: Span::Backed {
                            start: self.start,
                            len: i - self.start + 1,
                        },
                    });
                    self.state = TokenizerState::Start;
                    return self.buffer.pop_front();
                }
                // If we are not ending the quoted string, just keep going.
            } else if c.is_whitespace() {
                match self.state {
                    TokenizerState::Start => {
                        self.state = TokenizerState::InWhitespace;
                        self.start = i;
                    }
                    TokenizerState::InWhitespace => (),
                    TokenizerState::InComment => {
                        if c == '\n' {
                            // End the comment
                            self.buffer.push_back(Element {
                                token: Token::Comment,
                                span: Span::Backed {
                                    start: self.start,
                                    len: i - self.start + 1,
                                },
                            });
                            self.state = TokenizerState::Start;
                            self.start = i;
                            return self.buffer.pop_front();
                        }
                    }
                    TokenizerState::InQuote => (),
                    TokenizerState::InIdentifier => {
                        // We just hit whitespace so finish an identifier.
                        self.buffer.push_back(Element {
                            token: Token::Identifier,
                            span: Span::Backed {
                                start: self.start,
                                len: i - self.start,
                            },
                        });
                        self.state = TokenizerState::InWhitespace;
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
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                            TokenizerState::InComment => {
                                // Don't start a quote if we are in a comment.
                                continue;
                            }
                            TokenizerState::InQuote => (),
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        // Then we start a quote
                        self.state = TokenizerState::InQuote;
                        self.start = i;
                    }
                    '!' => {
                        // We have begun a quote, this continues until the end of the line
                        match self.state {
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Identifier,
                                        span: Span::Backed {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            TokenizerState::InComment => {
                                // Don't start a comment if we are already in a comment.
                                continue;
                            }
                            TokenizerState::InQuote => unreachable!("cannot be quote"),
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        // Then we start a comment
                        self.state = TokenizerState::InComment;
                        self.start = i;
                    }
                    '=' => {
                        match self.state {
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span::Backed {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            TokenizerState::InComment => {
                                continue;
                            }
                            TokenizerState::InQuote => {
                                continue;
                            }
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::Equals,
                            span: Span::Backed { start: i, len: 1 },
                        });
                        self.state = TokenizerState::Start;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                    ',' => {
                        match self.state {
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span::Backed {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            TokenizerState::InComment => {
                                continue;
                            }
                            TokenizerState::InQuote => {
                                continue;
                            }
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::Comma,
                            span: Span::Backed { start: i, len: 1 },
                        });
                        self.state = TokenizerState::Start;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                    '(' => {
                        match self.state {
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span::Backed {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            TokenizerState::InComment => {
                                continue;
                            }
                            TokenizerState::InQuote => {
                                continue;
                            }
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::LeftParen,
                            span: Span::Backed { start: i, len: 1 },
                        });
                        self.state = TokenizerState::Start;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                    ')' => {
                        match self.state {
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span::Backed {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            TokenizerState::InComment => {
                                continue;
                            }
                            TokenizerState::InQuote => {
                                continue;
                            }
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::RightParen,
                            span: Span::Backed { start: i, len: 1 },
                        });
                        self.state = TokenizerState::Start;
                        self.start = i;
                        return self.buffer.pop_front();
                    }
                    ':' => {
                        match self.state {
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span::Backed {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            TokenizerState::InComment => {
                                continue;
                            }
                            TokenizerState::InQuote => {
                                continue;
                            }
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::Colon,
                            span: Span::Backed { start: i, len: 1 },
                        });
                        self.state = TokenizerState::Start;
                        self.start = i + 1;
                        return self.buffer.pop_front();
                    }
                    '/' => {
                        match self.state {
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span::Backed {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            TokenizerState::InComment => {
                                continue;
                            }
                            TokenizerState::InQuote => {
                                continue;
                            }
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::RightSlash,
                            span: Span::Backed { start: i, len: 1 },
                        });
                        self.state = TokenizerState::Start;
                        self.start = i + 1;
                        return self.buffer.pop_front();
                    }
                    '&' => {
                        match self.state {
                            TokenizerState::Start => (),
                            TokenizerState::InWhitespace => {
                                {
                                    // We just hit whitespace so finish an identifier.
                                    self.buffer.push_back(Element {
                                        token: Token::Whitespace,
                                        span: Span::Backed {
                                            start: self.start,
                                            len: i - self.start,
                                        },
                                    });
                                }
                            }
                            TokenizerState::InComment => {
                                continue;
                            }
                            TokenizerState::InQuote => {
                                continue;
                            }
                            TokenizerState::InIdentifier => {
                                // We just hit whitespace so finish an identifier.
                                self.buffer.push_back(Element {
                                    token: Token::Identifier,
                                    span: Span::Backed {
                                        start: self.start,
                                        len: i - self.start,
                                    },
                                });
                            }
                        }
                        self.buffer.push_back(Element {
                            token: Token::Ampersand,
                            span: Span::Backed { start: i, len: 1 },
                        });
                        self.state = TokenizerState::Start;
                        self.start = i + 1;
                        return self.buffer.pop_front();
                    }
                    _ => match self.state {
                        TokenizerState::Start => {
                            self.state = TokenizerState::InIdentifier;
                            self.start = i;
                        }
                        TokenizerState::InWhitespace => {
                            self.buffer.push_back(Element {
                                token: Token::Whitespace,
                                span: Span::Backed {
                                    start: self.start,
                                    len: i - self.start,
                                },
                            });
                            self.state = TokenizerState::InIdentifier;
                            self.start = i;
                            return self.buffer.pop_front();
                        }
                        TokenizerState::InComment => {
                            continue;
                        }
                        TokenizerState::InQuote => {
                            continue;
                        }
                        TokenizerState::InIdentifier => (),
                    },
                }
            }
            // if i
        }
        let len = self.input.len() - self.start;
        match self.state {
            TokenizerState::Start => (),
            TokenizerState::InWhitespace => {
                if len > 0 {
                    self.buffer.push_back(Element {
                        token: Token::Whitespace,
                        span: Span::Backed {
                            start: self.start,
                            len,
                        },
                    });
                    self.start += len;
                }
            }
            TokenizerState::InComment => {
                if len > 0 {
                    self.buffer.push_back(Element {
                        token: Token::Comment,
                        span: Span::Backed {
                            start: self.start,
                            len,
                        },
                    });
                    self.start += len;
                }
            }
            TokenizerState::InQuote => {
                panic!("incomplete quote");
            }
            TokenizerState::InIdentifier => {
                if len > 0 {
                    self.buffer.push_back(Element {
                        token: Token::Identifier,
                        span: Span::Backed {
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
        let input = std::fs::read_to_string(test_path).expect("could not open test file");
        let result = NamelistTokenizer::new(&input).tokenize_nml();
        assert_eq!(input, result.to_string());
    }

    #[test]
    fn basic_parse() {
        let test_path = "tests/test_input.txt";
        let input = std::fs::read_to_string(test_path).expect("could not open test file");
        let tokens = NamelistTokenizer::new(&input);
        let parser = tokens.into_parser();
        let nml_file = parser.into_nml_file();
        assert_eq!(input, nml_file.to_string());
    }

    #[test]
    fn basic_parse_meshes() {
        let test_path = "tests/test_input.txt";
        let input = std::fs::read_to_string(test_path).expect("could not open test file");
        let tokens = NamelistTokenizer::new(&input);
        let parser = tokens.into_parser();
        let mut nml_file = parser.into_nml_file();
        // Find all meshes.
        let n_meshes = nml_file
            .elements
            .iter_mut()
            .filter(|elem| {
                if let NamelistElement::Namelist(nml) = elem {
                    nml.name == "MESH"
                } else {
                    false
                }
            })
            .count();
        assert_eq!(n_meshes, 2);
        assert_eq!(input, nml_file.to_string());
    }
}
