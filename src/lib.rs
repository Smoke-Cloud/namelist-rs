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
    if cs == b"t" || cs == b"T" {
        Ok((i, true))
    } else if cs == b"f" || cs == b"F" {
        Ok((i, false))
    } else {
        unreachable!()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let res = boolean(b"T");
        assert_eq!(res, (Ok((&[][..], true))));
    }
}
