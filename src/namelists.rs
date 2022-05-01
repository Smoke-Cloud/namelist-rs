use crate::{
    tokenizer::{LocatedToken, Token},
    Namelist,
};

pub fn parse_namelist(tokens: Vec<LocatedToken>) -> Option<Namelist> {
    if tokens.get(0).map(|x| x.token()) != Some(&Token::Ampersand) {
        return None;
    }
    Some(Namelist { tokens })
}
