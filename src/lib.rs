#[macro_use]
extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while_m_n, is_a},
    character::complete::{one_of, char, none_of},
    character::is_digit,
    combinator::{map, not, opt},
    multi::{many0, many1},
    sequence::{preceded, terminated},
    IResult,
};

pub struct NamelistFile {

}

/// A boolean is either an F or T (case insensitive) followed by any series of
/// non-whitespace characters. It may also pre prepended by a period.
pub fn boolean(i: &[u8]) -> IResult<&[u8], bool> {
    let (i, _) = opt(char('.'))(i)?;
    let (i, cs) = is_a("tTfF")(i)?;
    let (i, _) = many0(none_of(" \t\r\n/,"))(i)?;
    match cs {
        b"t" | b"T" => Ok((i, true)),
        b"f" | b"F" => Ok((i, false)),
        _ => unreachable!(),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    // In these tests Ok(remaining, result) is used to make sure that we have
    // consumed the input we expect to consume.

    #[test]
    fn boolean_examples() {
        assert_eq!(boolean(b"t"), Ok((&[][..], true)));
        assert_eq!(boolean(b"T"), Ok((&[][..], true)));
        assert_eq!(boolean(b"f"), Ok((&[][..], false)));
        assert_eq!(boolean(b"F"), Ok((&[][..], false)));
        assert_eq!(boolean(b".FALSE."), Ok((&[][..], false)));
        assert_eq!(boolean(b".TRUE."), Ok((&[][..], true)));
        let r: &[u8] = b", ";
        assert_eq!(boolean(b".TRUE., "), Ok((r, true)));
    }
}
