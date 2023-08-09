use std::{
    collections::VecDeque,
    fmt::Display,
    io::{Cursor, Read},
};
use utf8::{self, BufReadDecoder, BufReadDecoderError};

// TODO: Add line and column numbers
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub lo: usize,
    pub len: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocatedToken {
    /// Location of the token in the original file or stream.
    pub span: Option<Span>,
    pub token: Token,
}

impl LocatedToken {
    pub fn token(&self) -> &Token {
        &self.token
    }
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

impl Display for LocatedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token {
    LeftBracket,
    RightBracket,
    Equals,
    Colon,
    Comma,
    RightSlash,
    Ampersand,
    NewLine,
    /// Some variable string that forms a token. Currently this could also
    /// include numbers.
    QuotedStr(String),
    Bool(String),
    Whitespace(String),
    Identifier(String),
    Number(String),
    Comment(String),
}
impl Token {
    pub fn is_location_token(&self) -> bool {
        match self {
            Self::LeftBracket => true,
            Self::RightBracket => true,
            Self::Equals => false,
            Self::Colon => true,
            Self::Comma => true,
            Self::RightSlash => false,
            Self::Ampersand => false,
            Self::NewLine => true,
            Self::Bool(_) => false,
            Self::QuotedStr(_) => false,
            Self::Whitespace(_) => true,
            Self::Identifier(_) => false,
            Self::Number(_) => true,
            Self::Comment(_) => true,
        }
    }

    pub fn is_whitespace(&self) -> bool {
        matches!(self, Self::Whitespace(_) | Self::NewLine)
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, Self::Comment(_))
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftBracket => write!(f, "("),
            Self::RightBracket => write!(f, ")"),
            Self::Equals => write!(f, "="),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::RightSlash => write!(f, "/"),
            Self::Ampersand => write!(f, "&"),
            Self::NewLine => writeln!(f),
            Self::Bool(s) => write!(f, "{s}"),
            Self::QuotedStr(s) => write!(f, "{s}"),
            Self::Whitespace(s) => write!(f, "{s}"),
            Self::Identifier(s) => write!(f, "{s}"),
            Self::Number(s) => write!(f, "{s}"),
            Self::Comment(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenizerState {
    Start,
    StartInNamelist,
    InQuote { start: usize, content: String },
    InIdentifier { start: usize, content: String },
    InBoolOrNumber { start: usize, content: String },
    InNumber { start: usize, content: String },
    InBool { start: usize, content: String },
    InWhitespace { start: usize, content: String },
    Comment { start: usize, content: String },
}

pub struct CharDecoder<R: std::io::Read> {
    iter: BufReadDecoder<std::io::BufReader<R>>,
    offset: usize,
    chars: VecDeque<(usize, char)>,
}

impl<R: Read> CharDecoder<R> {
    pub fn new(input: R) -> Self {
        Self {
            iter: BufReadDecoder::new(std::io::BufReader::new(input)),
            chars: VecDeque::new(),
            offset: 0,
        }
    }
}

impl<R: Read> Iterator for CharDecoder<R> {
    type Item = Result<(usize, char), CharDecodeError>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(res) = self.chars.pop_front() {
                return Some(Ok(res));
            } else {
                match self.iter.next_strict()? {
                    Ok(next_string) => {
                        let offset = self.offset;
                        for r in next_string.char_indices().map(|(i, c)| (i + offset, c)) {
                            self.chars.push_back(r);
                        }
                        self.offset += next_string.len();
                    }
                    Err(BufReadDecoderError::InvalidByteSequence(s)) => {
                        return Some(Err(CharDecodeError::DecodeError(s.into())))
                    }
                    Err(BufReadDecoderError::Io(err)) => {
                        return Some(Err(CharDecodeError::IoError(err)))
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum CharDecodeError {
    IoError(std::io::Error),
    DecodeError(Vec<u8>),
}

impl std::fmt::Display for CharDecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::IoError(_) => {
                write!(f, "Invalid bool or number")
            }
            Self::DecodeError(_) => {
                write!(f, "Invalid character")
            }
        }
    }
}

impl std::error::Error for CharDecodeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            Self::IoError(_) => None,
            Self::DecodeError(_) => None,
        }
    }
}

pub struct TokenIter<B: std::io::Read> {
    iter: CharDecoder<std::io::BufReader<B>>,
    buf: Option<(usize, char)>,
    state: TokenizerState,
    line: usize,
    column: usize,
}

impl<R: std::io::Read> TokenIter<R> {
    pub fn new(input: R) -> Self {
        Self {
            iter: CharDecoder::new(std::io::BufReader::new(input)),
            buf: None,
            state: TokenizerState::Start,
            line: 0,
            column: 0,
        }
    }
    fn pos_advance_token(&mut self, token: &Token) {
        match token {
            Token::LeftBracket => self.pos_advance('('),
            Token::RightBracket => self.pos_advance(')'),
            Token::Equals => self.pos_advance('='),
            Token::Colon => self.pos_advance(':'),
            Token::Comma => self.pos_advance(','),
            Token::RightSlash => self.pos_advance('/'),
            Token::Ampersand => self.pos_advance('&'),
            Token::NewLine => self.pos_advance('\n'),
            Token::QuotedStr(s)
            | Token::Bool(s)
            | Token::Whitespace(s)
            | Token::Identifier(s)
            | Token::Number(s)
            | Token::Comment(s) => {
                for c in s.chars() {
                    self.pos_advance(c);
                }
            }
        }
    }
    fn pos_advance(&mut self, c: char) {
        if c == '\n' {
            self.column = 0;
            self.line += 1;
        } else {
            self.column += 1
        }
    }
}

impl<R: std::io::Read> Iterator for TokenIter<R> {
    type Item = Result<LocatedToken, TokenizerError>;
    fn next(&mut self) -> Option<Self::Item> {
        let token = loop {
            match self.buf.take().map(Ok).or_else(|| self.iter.next()) {
                Some(Ok((i, c))) => {
                    let line = self.line;
                    let column = self.column;
                    match &mut self.state {
                        TokenizerState::Start => {
                            if c == '&' {
                                // If a new line starts with an ampersand we should
                                // interpret what follows as a namelist.
                                let token = Token::Ampersand;
                                let span = Some(Span {
                                    lo: i,
                                    len: 1,
                                    column,
                                    line,
                                });
                                let token = LocatedToken { span, token };
                                self.state = TokenizerState::StartInNamelist;
                                break Some(Ok(token));
                            } else if c == '/' {
                                let len = 1;
                                let token = LocatedToken {
                                    span: Some(Span {
                                        lo: i,
                                        len,
                                        column,
                                        line,
                                    }),
                                    token: Token::RightSlash,
                                };
                                self.state = TokenizerState::Start;
                                break Some(Ok(token));
                            } else if c == '\n' {
                                let len = 1;
                                let token = LocatedToken {
                                    span: Some(Span {
                                        lo: i,
                                        len,
                                        column,
                                        line,
                                    }),
                                    token: Token::Comment(c.to_string()),
                                };
                                self.state = TokenizerState::Start;
                                break Some(Ok(token));
                            } else {
                                // Otherwise it is just 'junk' or comment.
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                self.state = TokenizerState::Comment { start, content };
                            }
                        }
                        TokenizerState::StartInNamelist => {
                            if c.is_whitespace() {
                                let start = i;
                                let mut content = String::new();
                                content.push(c);
                                self.state = TokenizerState::InWhitespace { start, content };
                            } else {
                                match c {
                                    '\'' => {
                                        let start = i;
                                        let mut content = String::new();
                                        content.push(c);
                                        self.state = TokenizerState::InQuote { start, content };
                                    }
                                    '.' => {
                                        let start = i;
                                        let mut content = String::new();
                                        content.push(c);
                                        self.state =
                                            TokenizerState::InBoolOrNumber { start, content };
                                    }
                                    '!' => {
                                        let start = i;
                                        let mut content = String::new();
                                        content.push(c);
                                        self.state = TokenizerState::Comment { start, content };
                                    }
                                    '=' => {
                                        let token = Token::Equals;
                                        let span = Some(Span {
                                            lo: i,
                                            len: 1,
                                            column,
                                            line,
                                        });
                                        let token = LocatedToken { span, token };
                                        self.state = TokenizerState::StartInNamelist;
                                        break Some(Ok(token));
                                    }
                                    '(' => {
                                        let token = Token::LeftBracket;
                                        let span = Some(Span {
                                            lo: i,
                                            len: 1,
                                            column,
                                            line,
                                        });
                                        let token = LocatedToken { span, token };
                                        self.state = TokenizerState::StartInNamelist;
                                        break Some(Ok(token));
                                    }
                                    ')' => {
                                        let token = Token::RightBracket;
                                        let span = Some(Span {
                                            lo: i,
                                            len: 1,
                                            column,
                                            line,
                                        });
                                        let token = LocatedToken { span, token };
                                        self.state = TokenizerState::StartInNamelist;
                                        break Some(Ok(token));
                                    }
                                    ':' => {
                                        let token = Token::Colon;
                                        let span = Some(Span {
                                            lo: i,
                                            len: 1,
                                            column,
                                            line,
                                        });
                                        let token = LocatedToken { span, token };
                                        self.state = TokenizerState::StartInNamelist;
                                        break Some(Ok(token));
                                    }
                                    ',' => {
                                        let token = Token::Comma;
                                        let span = Some(Span {
                                            lo: i,
                                            len: 1,
                                            column,
                                            line,
                                        });
                                        let token = LocatedToken { span, token };
                                        self.state = TokenizerState::StartInNamelist;
                                        break Some(Ok(token));
                                    }
                                    '/' => {
                                        let token = Token::RightSlash;
                                        let span = Some(Span {
                                            lo: i,
                                            len: 1,
                                            column,
                                            line,
                                        });
                                        let token = LocatedToken { span, token };
                                        self.state = TokenizerState::Start;
                                        break Some(Ok(token));
                                    }
                                    '&' => {
                                        let token = Token::Ampersand;
                                        let span = Some(Span {
                                            lo: i,
                                            len: 1,
                                            column,
                                            line,
                                        });
                                        let token = LocatedToken { span, token };
                                        self.state = TokenizerState::StartInNamelist;
                                        break Some(Ok(token));
                                    }
                                    _ => {
                                        if c.is_alphabetic() {
                                            let start = i;
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state =
                                                TokenizerState::InIdentifier { start, content };
                                        } else if c.is_whitespace() {
                                            let start = i;
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state =
                                                TokenizerState::InWhitespace { start, content };
                                        } else if c.is_ascii_digit() || c == '-' {
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state =
                                                TokenizerState::InNumber { start: i, content };
                                        } else {
                                            let start = i;
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state = TokenizerState::Comment { start, content };
                                        }
                                    }
                                }
                            }
                        }
                        TokenizerState::InQuote { start, content } => match c {
                            '\'' => {
                                content.push(c);
                                let len = content.len();
                                let value = std::mem::take(content);
                                let token = LocatedToken {
                                    span: Some(Span {
                                        lo: *start,
                                        len,
                                        column,
                                        line,
                                    }),
                                    token: Token::QuotedStr(value),
                                };
                                self.state = TokenizerState::StartInNamelist;
                                break Some(Ok(token));
                            }
                            _ => {
                                content.push(c);
                            }
                        },
                        TokenizerState::InBoolOrNumber { start, content } => {
                            if c.is_ascii_digit() {
                                content.push(c);
                                let value = std::mem::take(content);
                                self.state = TokenizerState::InNumber {
                                    start: *start,
                                    content: value,
                                };
                            } else {
                                match c {
                                    'T' | 't' | 'F' | 'f' => {
                                        content.push(c);
                                        let value = std::mem::take(content);
                                        self.state = TokenizerState::InBool {
                                            start: *start,
                                            content: value,
                                        };
                                    }
                                    _ => {
                                        content.push(c);
                                        return Some(Err(TokenizerError::InvalidBoolOrNumber(
                                            Span {
                                                lo: *start,
                                                len: content.len(),
                                                line,
                                                column,
                                            },
                                        )));
                                    }
                                }
                            }
                        }
                        TokenizerState::InBool { start, content } => match c {
                            '.' => {
                                content.push(c);
                                let len = content.len();
                                let value = std::mem::take(content);
                                let token = LocatedToken {
                                    span: Some(Span {
                                        lo: *start,
                                        len,
                                        column,
                                        line,
                                    }),
                                    token: Token::Bool(value),
                                };
                                self.state = TokenizerState::StartInNamelist;
                                break Some(Ok(token));
                            }
                            _ => {
                                content.push(c);
                            }
                        },
                        TokenizerState::Comment { start, content } => match c {
                            '\n' => {
                                // If we come to a new line while processing
                                // comments, we revert to start. Even an ampersand
                                // does not break us out of a comment.
                                content.push(c);
                                let len = content.len();
                                let value = std::mem::take(content);
                                let token = LocatedToken {
                                    span: Some(Span {
                                        lo: *start,
                                        len,
                                        column,
                                        line,
                                    }),
                                    token: Token::Comment(value),
                                };
                                self.state = TokenizerState::Start;
                                break Some(Ok(token));
                            }
                            _ => {
                                content.push(c);
                            }
                        },
                        TokenizerState::InWhitespace { start, content } => {
                            if c.is_whitespace() {
                                content.push(c);
                            } else {
                                let len = content.len();
                                let value = std::mem::take(content);
                                let token = LocatedToken {
                                    span: Some(Span {
                                        lo: *start,
                                        len,
                                        column,
                                        line,
                                    }),
                                    token: Token::Whitespace(value),
                                };
                                match c {
                                    '\'' => {
                                        let start = i;
                                        let mut content = String::new();
                                        content.push(c);
                                        self.state = TokenizerState::InQuote { start, content };
                                    }
                                    '=' | '(' | ')' | ':' | ',' | '/' | '&' => {
                                        self.buf.replace((i, c));
                                        self.state = TokenizerState::StartInNamelist;
                                    }
                                    _ => {
                                        if c.is_alphabetic() {
                                            let start = i;
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state =
                                                TokenizerState::InIdentifier { start, content };
                                        } else if c.is_whitespace() {
                                            let start = i;
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state =
                                                TokenizerState::InWhitespace { start, content };
                                        } else if c == '.' {
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state = TokenizerState::InBoolOrNumber {
                                                start: i,
                                                content,
                                            };
                                        } else if c.is_ascii_digit()
                                            || c == 'e'
                                            || c == 'E'
                                            || c == '-'
                                            || c == '+'
                                        {
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state =
                                                TokenizerState::InNumber { start: i, content };
                                        } else if c == '!' {
                                            let start = i;
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state = TokenizerState::Comment { start, content };
                                        } else {
                                            return Some(Err(TokenizerError::InvalidCharacter(
                                                Span {
                                                    lo: *start,
                                                    len: content.len(),
                                                    line,
                                                    column,
                                                },
                                            )));
                                        }
                                    }
                                }
                                break Some(Ok(token));
                            }
                        }
                        TokenizerState::InIdentifier { start, content } => {
                            if c.is_alphanumeric() || c == '_' {
                                content.push(c);
                            } else {
                                let len = content.len();
                                let value = std::mem::take(content);
                                let span = Some(Span {
                                    lo: *start,
                                    len,
                                    column,
                                    line,
                                });
                                let token = Token::Identifier(value);
                                self.buf.replace((i, c));
                                self.state = TokenizerState::StartInNamelist;
                                let token = LocatedToken { span, token };
                                break Some(Ok(token));
                            }
                        }
                        TokenizerState::InNumber { start, content } => {
                            if c.is_ascii_digit()
                                || c == '.'
                                || c == 'e'
                                || c == 'E'
                                || c == '-'
                                || c == '+'
                            {
                                content.push(c);
                            } else {
                                let len = content.len();
                                let value = std::mem::take(content);
                                let token = LocatedToken {
                                    span: Some(Span {
                                        lo: *start,
                                        len,
                                        column,
                                        line,
                                    }),
                                    token: Token::Number(value),
                                };
                                match c {
                                    '\'' => {
                                        let start = i;
                                        let mut content = String::new();
                                        content.push(c);
                                        self.state = TokenizerState::InQuote { start, content };
                                    }
                                    '=' | '(' | ')' | ':' | ',' | '&' | '!' => {
                                        self.buf.replace((i, c));
                                        self.state = TokenizerState::StartInNamelist;
                                    }
                                    '/' => {
                                        self.buf.replace((i, c));
                                        self.state = TokenizerState::Start;
                                    }
                                    _ => {
                                        if c.is_alphabetic() {
                                            let start = i;
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state =
                                                TokenizerState::InIdentifier { start, content };
                                        } else if c.is_whitespace() {
                                            let start = i;
                                            let mut content = String::new();
                                            content.push(c);
                                            self.state =
                                                TokenizerState::InWhitespace { start, content };
                                        } else {
                                            return Some(Err(TokenizerError::InvalidCharacter(
                                                Span {
                                                    lo: *start,
                                                    len: content.len(),
                                                    line,
                                                    column,
                                                },
                                            )));
                                        }
                                    }
                                }
                                break Some(Ok(token));
                            }
                        }
                    }
                }
                Some(Err(err)) => {
                    let line = self.line;
                    let column = self.column;
                    return Some(Err(TokenizerError::CharError(
                        Span {
                            lo: 0,
                            len: 0,
                            line,
                            column,
                        },
                        err,
                    )));
                }
                None => {
                    let line = self.line;
                    let column = self.column;
                    // We have reached EOF
                    match &mut self.state {
                        TokenizerState::Start | TokenizerState::StartInNamelist => {
                            break None;
                        }
                        TokenizerState::InQuote { start, content } => {
                            return Some(Err(TokenizerError::UnclosedQuote(Span {
                                lo: *start,
                                len: content.len(),
                                line,
                                column,
                            })));
                        }
                        TokenizerState::InBool { start, content } => {
                            return Some(Err(TokenizerError::UnfinishedBool(Span {
                                lo: *start,
                                len: content.len(),
                                line,
                                column,
                            })));
                        }
                        TokenizerState::InBoolOrNumber { start, content } => {
                            return Some(Err(TokenizerError::UnfinishedBoolOrNumber(Span {
                                lo: *start,
                                len: content.len(),
                                line,
                                column,
                            })));
                        }
                        TokenizerState::InWhitespace { start, content } => {
                            let len = content.len();
                            let value = std::mem::take(content);
                            let token = Token::Whitespace(value);
                            let span = Some(Span {
                                lo: *start,
                                len,
                                column,
                                line,
                            });
                            self.state = TokenizerState::StartInNamelist;
                            let token = LocatedToken { span, token };
                            break Some(Ok(token));
                        }
                        TokenizerState::Comment { start, content } => {
                            let len = content.len();
                            let value = std::mem::take(content);
                            let token = Token::Comment(value);
                            let span = Some(Span {
                                lo: *start,
                                len,
                                column,
                                line,
                            });
                            self.state = TokenizerState::Start;
                            let token = LocatedToken { span, token };
                            break Some(Ok(token));
                        }
                        TokenizerState::InIdentifier { start, content } => {
                            let len = content.len();
                            let value = std::mem::take(content);
                            let token = Token::Identifier(value);
                            let span = Some(Span {
                                lo: *start,
                                len,
                                column,
                                line,
                            });
                            self.state = TokenizerState::StartInNamelist;
                            let token = LocatedToken { span, token };
                            break Some(Ok(token));
                        }
                        TokenizerState::InNumber { start, content } => {
                            let len = content.len();
                            let value = std::mem::take(content);
                            let token = Token::Number(value);
                            let span = Some(Span {
                                lo: *start,
                                len,
                                column,
                                line,
                            });
                            self.state = TokenizerState::StartInNamelist;
                            let token = LocatedToken { span, token };
                            break Some(Ok(token));
                        }
                    }
                }
            }
        };
        if let Some(Ok(ref token)) = token {
            self.pos_advance_token(&token.token);
        }
        token
    }
}

pub fn tokenize_reader<R: Read>(input: R) -> Result<Vec<LocatedToken>, TokenizerError> {
    let mut tokens = vec![];
    for token in TokenIter::new(input) {
        tokens.push(token?);
    }
    Ok(tokens)
}

pub fn tokenize_str(input: &str) -> Result<Vec<LocatedToken>, TokenizerError> {
    let input = Cursor::new(input);
    let mut tokens = vec![];
    for token in TokenIter::new(input) {
        tokens.push(token?);
    }
    Ok(tokens)
}

#[derive(Debug)]
pub enum NmlParseError {
    Tokenize(TokenizerError),
    InvalidParameterName(Option<Span>),
    NoAmpersand(Option<Span>),
    InvalidGroupName(Option<Span>),
    NoEquals(Option<Span>),
    NoTokens,
    // Parse(Option<Span>, String),
    // Io(std::io::Error),
}

impl NmlParseError {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Tokenize(err) => Some(err.span()),
            Self::InvalidParameterName(span) => *span,
            Self::NoAmpersand(span) => *span,
            Self::InvalidGroupName(span) => *span,
            Self::NoEquals(span) => *span,
            Self::NoTokens => None,
            // Self::Parse(span, _) => *span,
            // Self::Io(_) => None,
        }
    }
}

impl std::fmt::Display for NmlParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tokenize(err) => {
                write!(f, "{err}")
            }
            Self::InvalidParameterName(_) => {
                write!(f, "invalid parameter name")
            }
            Self::NoAmpersand(_) => {
                write!(f, "no ampersand at the beginning of namelist")
            }
            Self::InvalidGroupName(_) => {
                write!(f, "no ampersand at the beginning of namelist")
            }
            Self::NoEquals(_) => {
                write!(f, "no equals succeeding the parameter name")
            }
            Self::NoTokens => {
                write!(f, "namelist ended early with insufficient tokens")
            } // Self::Parse(_, err) => {
              //     write!(f, "{err}")
              // }
              // Self::Io(err) => {
              //     write!(f, "{err}")
              // }
        }
    }
}

impl std::error::Error for NmlParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Tokenize(err) => Some(err),
            Self::InvalidParameterName(_) => None,
            Self::NoAmpersand(_) => None,
            Self::InvalidGroupName(_) => None,
            Self::NoEquals(_) => None,
            Self::NoTokens => None,
            // Self::Parse(_, _) => None,
            // Self::Io(err) => Some(err),
        }
    }
}

#[derive(Debug)]
pub enum TokenizerError {
    InvalidBoolOrNumber(Span),
    InvalidCharacter(Span),
    // InvalidParameterName(Span),
    UnfinishedBool(Span),
    UnfinishedBoolOrNumber(Span),
    UnclosedQuote(Span),
    CharError(Span, CharDecodeError),
}

impl TokenizerError {
    pub fn span(&self) -> Span {
        match self {
            Self::InvalidBoolOrNumber(span) => *span,
            Self::InvalidCharacter(span) => *span,
            Self::UnfinishedBool(span) => *span,
            Self::UnfinishedBoolOrNumber(span) => *span,
            Self::UnclosedQuote(span) => *span,
            Self::CharError(span, _) => *span,
        }
    }
}

impl std::fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidBoolOrNumber(_) => {
                write!(f, "Invalid bool or number")
            }
            Self::InvalidCharacter(_) => {
                write!(f, "Invalid character")
            }
            Self::UnfinishedBool(_) => {
                write!(f, "Unfinished bool")
            }
            Self::UnfinishedBoolOrNumber(_) => {
                write!(f, "Unfinished booll or number")
            }
            Self::UnclosedQuote(_) => {
                write!(f, "Unclosed quote")
            }
            Self::CharError(_, err) => {
                write!(f, "UTF-8 decode error: {err}")
            }
        }
    }
}

impl std::error::Error for TokenizerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            Self::InvalidBoolOrNumber(_) => None,
            Self::InvalidCharacter(_) => None,
            Self::UnfinishedBool(_) => None,
            Self::UnfinishedBoolOrNumber(_) => None,
            Self::UnclosedQuote(_) => None,
            Self::CharError(_, _) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trivial_tokens0() {
        let s = "abc=2";
        let tokens = tokenize_str(s).expect("test tokenization failed");
        assert_eq!(
            vec![LocatedToken {
                span: Some(Span {
                    lo: 0,
                    len: 5,
                    column: 0,
                    line: 0
                }),
                token: Token::Comment("abc=2".to_string()),
            },],
            tokens
        );
    }

    #[test]
    fn trivial_tokens1() {
        let s = "&H abc=2";
        let tokens: Vec<_> = tokenize_str(s)
            .expect("test tokenization failed")
            .into_iter()
            .map(|x| x.token)
            .collect();
        assert_eq!(
            vec![
                Token::Ampersand,
                Token::Identifier("H".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Identifier("abc".to_string()),
                Token::Equals,
                Token::Number("2".to_string()),
            ],
            tokens
        );
    }

    #[test]
    fn trivial_tokens2() {
        let s = "&H abc= 2";
        let tokens: Vec<_> = tokenize_str(s)
            .expect("test tokenization failed")
            .into_iter()
            .map(|x| x.token)
            .collect();
        assert_eq!(
            vec![
                Token::Ampersand,
                Token::Identifier("H".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Identifier("abc".to_string()),
                Token::Equals,
                Token::Whitespace(" ".to_string()),
                Token::Number("2".to_string()),
            ],
            tokens
        );
    }

    #[test]
    fn trivial_tokens3() {
        assert_eq!(
            tokenize_str("&H )=2")
                .expect("test tokenization failed")
                .into_iter()
                .map(|x| x.token)
                .collect::<Vec<_>>(),
            vec![
                Token::Ampersand,
                Token::Identifier("H".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::RightBracket,
                Token::Equals,
                Token::Number("2".to_string()),
            ]
        );
    }

    #[test]
    fn trivial_tokens4() {
        let s = "&abc=2/";
        let tokens: Vec<_> = tokenize_str(s)
            .expect("test tokenization failed")
            .into_iter()
            .map(|x| x.token)
            .collect();
        assert_eq!(
            vec![
                Token::Ampersand,
                Token::Identifier("abc".to_string()),
                Token::Equals,
                Token::Number("2".to_string()),
                Token::RightSlash,
            ],
            tokens
        );
    }

    #[test]
    fn trivial_tokens5() {
        let s = "&abc=.2/";
        let tokens: Vec<_> = tokenize_str(s)
            .expect("test tokenization failed")
            .into_iter()
            .map(|x| x.token)
            .collect();
        assert_eq!(
            vec![
                Token::Ampersand,
                Token::Identifier("abc".to_string()),
                Token::Equals,
                Token::Number(".2".to_string()),
                Token::RightSlash,
            ],
            tokens
        );
    }

    #[test]
    fn trivial_tokens6() {
        let s = "&abc=2./";
        let tokens: Vec<_> = tokenize_str(s)
            .expect("test tokenization failed")
            .into_iter()
            .map(|x| x.token)
            .collect();
        assert_eq!(
            vec![
                Token::Ampersand,
                Token::Identifier("abc".to_string()),
                Token::Equals,
                Token::Number("2.".to_string()),
                Token::RightSlash,
            ],
            tokens
        );
    }
    #[test]
    fn trivial_tokens7() {
        let s = "&abc=2.\n/";
        let tokens: Vec<_> = tokenize_str(s)
            .expect("test tokenization failed")
            .into_iter()
            .map(|x| x.token)
            .collect();
        assert_eq!(
            vec![
                Token::Ampersand,
                Token::Identifier("abc".to_string()),
                Token::Equals,
                Token::Number("2.".to_string()),
                Token::Whitespace("\n".to_string()),
                Token::RightSlash,
            ],
            tokens
        );
    }
    #[test]
    fn trivial_tokens8() {
        let s = "&abc=2.\r\n/";
        let tokens: Vec<_> = tokenize_str(s)
            .expect("test tokenization failed")
            .into_iter()
            .map(|x| x.token)
            .collect();
        assert_eq!(
            vec![
                Token::Ampersand,
                Token::Identifier("abc".to_string()),
                Token::Equals,
                Token::Number("2.".to_string()),
                Token::Whitespace("\r\n".to_string()),
                Token::RightSlash,
            ],
            tokens
        );
    }

    #[test]
    fn bad_tokens1() {
        let res = tokenize_str("&H abc=.TR");
        assert!(res.is_err());
        if let Err(TokenizerError::UnfinishedBool(span)) = res {
            assert_eq!(span.lo, 7);
            assert_eq!(span.len, 3);
            assert_eq!(span.line, 0);
            assert_eq!(span.column, 7);
        } else {
            panic!("Incorrect error type {:?}", res);
        }
    }

    #[test]
    fn simple_tokens1() {
        let s = "&H abc=2,'ad c' (2,:)";
        let tokens: Vec<_> = tokenize_str(s)
            .expect("test tokenization failed")
            .into_iter()
            .map(|x| x.token)
            .collect();
        let expected = vec![
            Token::Ampersand,
            Token::Identifier("H".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("abc".to_string()),
            Token::Equals,
            Token::Number("2".to_string()),
            Token::Comma,
            Token::QuotedStr("'ad c'".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::LeftBracket,
            Token::Number("2".to_string()),
            Token::Comma,
            Token::Colon,
            Token::RightBracket,
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn simple_tokens2() {
        assert_eq!(
            tokenize_str("&H TEMPERATURES(1:2)=273.15, 274")
                .expect("test tokenization failed")
                .into_iter()
                .map(|x| x.token)
                .collect::<Vec<_>>(),
            vec![
                Token::Ampersand,
                Token::Identifier("H".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Identifier("TEMPERATURES".to_string()),
                Token::LeftBracket,
                Token::Number("1".to_string()),
                Token::Colon,
                Token::Number("2".to_string()),
                Token::RightBracket,
                Token::Equals,
                Token::Number("273.15".to_string()),
                Token::Comma,
                Token::Whitespace(" ".to_string()),
                Token::Number("274".to_string()),
            ]
        );
    }

    #[test]
    fn simple_tokens3() {
        assert_eq!(
            tokenize_str("&H TEMPERATURES(1:2)=273.15, \n 274")
                .expect("test tokenization failed")
                .into_iter()
                .map(|x| x.token)
                .collect::<Vec<_>>(),
            vec![
                Token::Ampersand,
                Token::Identifier("H".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Identifier("TEMPERATURES".to_string()),
                Token::LeftBracket,
                Token::Number("1".to_string()),
                Token::Colon,
                Token::Number("2".to_string()),
                Token::RightBracket,
                Token::Equals,
                Token::Number("273.15".to_string()),
                Token::Comma,
                Token::Whitespace(" \n ".to_string()),
                Token::Number("274".to_string()),
            ]
        );
    }

    #[test]
    fn commented_tokens1() {
        let tokens: Vec<_> = tokenize_str("! hi\nTEMPERATURES(1:2)=273.15, \n 274")
            .expect("test tokenization failed")
            .into_iter()
            .map(|l_token| l_token.token().clone())
            .collect();
        let expected = vec![
            Token::Comment("! hi\n".to_string()),
            Token::Comment("TEMPERATURES(1:2)=273.15, \n".to_string()),
            Token::Comment(" 274".to_string()),
        ];
        assert_eq!(tokens, expected);
    }
    #[test]
    fn commented_tokens2() {
        let tokens: Vec<_> = tokenize_str("! hi\nTEMPERATURES(1:2)=273.15, \n 274 ! hello")
            .expect("test tokenization failed")
            .into_iter()
            .map(|l_token| l_token.token().clone())
            .collect();
        let expected = vec![
            Token::Comment("! hi\n".to_string()),
            Token::Comment("TEMPERATURES(1:2)=273.15, \n".to_string()),
            Token::Comment(" 274 ! hello".to_string()),
        ];
        assert_eq!(tokens, expected);
    }
}
