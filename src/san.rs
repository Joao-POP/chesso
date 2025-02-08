use crate::{Color, PieceType, Position, Square}; 

use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    sync::LazyLock,
};

type SANResult<T> = Result<T, SANError>;

static PARSE_TABLE: LazyLock<HashMap<Token, HashMap<char, usize>>> = LazyLock::new(|| {
    HashMap::from([
        (
            Token::NonTerminator(NonTerminator::M),
            HashMap::from([
                ('N', 1),
                ('K', 1),
                ('Q', 1),
                ('R', 1),
                ('B', 1),
                ('a', 1),
                ('b', 1),
                ('c', 1),
                ('d', 1),
                ('e', 1),
                ('f', 1),
                ('g', 1),
                ('h', 1),
                ('1', 1),
                ('0', 1),
                ('+', 1),
                ('-', 1),
                ('½', 1),
                ('O', 1),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::F),
            HashMap::from([
                ('a', 2),
                ('b', 2),
                ('c', 2),
                ('d', 2),
                ('e', 2),
                ('f', 2),
                ('g', 2),
                ('h', 2),
                ('1', 2),
                ('2', 2),
                ('3', 2),
                ('4', 2),
                ('5', 2),
                ('6', 2),
                ('7', 2),
                ('8', 2),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::R),
            HashMap::from([
                ('N', 3),
                ('K', 3),
                ('Q', 3),
                ('R', 3),
                ('B', 3),
                ('a', 3),
                ('b', 3),
                ('c', 3),
                ('d', 3),
                ('e', 3),
                ('f', 3),
                ('g', 3),
                ('h', 3),
                ('x', 3),
                (':', 3),
                ('+', 3),
                ('#', 3),
                ('=', 3),
                ('/', 3),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::B),
            HashMap::from([('-', 5)]),
        ),
        (
            Token::NonTerminator(NonTerminator::P),
            HashMap::from([
                ('a', 4),
                ('b', 4),
                ('c', 4),
                ('d', 4),
                ('e', 4),
                ('f', 4),
                ('g', 4),
                ('h', 4),
                ('1', 4),
                ('2', 4),
                ('3', 4),
                ('4', 4),
                ('5', 4),
                ('6', 4),
                ('7', 4),
                ('8', 4),
                ('x', 4),
                (':', 4),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::Phi),
            HashMap::from([('-', 6)]),
        ),
        (
            Token::NonTerminator(NonTerminator::Tau),
            HashMap::from([('-', 7)]),
        ),
        (
            Token::NonTerminator(NonTerminator::Kappa),
            HashMap::from([('/', 8)]),
        ),
        (
            Token::NonTerminator(NonTerminator::X),
            HashMap::from([
                ('a', 9),
                ('b', 9),
                ('c', 9),
                ('d', 9),
                ('e', 9),
                ('f', 9),
                ('g', 9),
                ('h', 9),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::Z),
            HashMap::from([('N', 10), ('K', 10), ('Q', 10), ('R', 10), ('B', 10)]),
        ),
        (
            Token::NonTerminator(NonTerminator::Lambda),
            HashMap::from([('+', 11), ('#', 11), ('X', 11)]),
        ),
        (
            Token::NonTerminator(NonTerminator::Pi),
            HashMap::from([
                ('1', 20),
                ('2', 20),
                ('3', 20),
                ('4', 20),
                ('5', 20),
                ('6', 20),
                ('7', 20),
                ('8', 20),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::B1),
            HashMap::from([('0', 12), ('O', 12)]),
        ),
        (
            Token::NonTerminator(NonTerminator::Phi1),
            HashMap::from([('1', 13), ('0', 13), ('½', 13)]),
        ),
        (
            Token::NonTerminator(NonTerminator::Tau1),
            HashMap::from([('0', 14), ('½', 14)]),
        ),
        (
            Token::NonTerminator(NonTerminator::Kappa1),
            HashMap::from([('+', 15), ('-', 15)]),
        ),
        (
            Token::NonTerminator(NonTerminator::X1),
            HashMap::from([
                ('a', 16),
                ('b', 16),
                ('c', 16),
                ('d', 16),
                ('e', 16),
                ('f', 16),
                ('g', 16),
                ('h', 16),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::B2),
            HashMap::from([('-', 17)]),
        ),
        (
            Token::NonTerminator(NonTerminator::S),
            HashMap::from([('O', 18), ('0', 18)]),
        ),
        (
            Token::NonTerminator(NonTerminator::X2),
            HashMap::from([
                ('a', 19),
                ('b', 19),
                ('c', 19),
                ('d', 19),
                ('e', 19),
                ('f', 19),
                ('g', 19),
                ('h', 19),
                ('1', 19),
                ('2', 19),
                ('3', 19),
                ('4', 19),
                ('5', 19),
                ('6', 19),
                ('7', 19),
                ('8', 19),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::F1),
            HashMap::from([
                ('1', 21),
                ('2', 21),
                ('3', 21),
                ('4', 21),
                ('5', 21),
                ('6', 21),
                ('7', 21),
                ('8', 21),
                ('+', 21),
                ('#', 21),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::F2),
            HashMap::from([
                ('a', 22),
                ('b', 22),
                ('c', 22),
                ('d', 22),
                ('e', 22),
                ('f', 22),
                ('g', 22),
                ('h', 22),
                ('1', 22),
                ('2', 22),
                ('3', 22),
                ('4', 22),
                ('5', 22),
                ('6', 22),
                ('7', 22),
                ('8', 22),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::F3),
            HashMap::from([
                ('1', 25),
                ('2', 25),
                ('3', 25),
                ('4', 25),
                ('5', 25),
                ('6', 25),
                ('7', 25),
                ('8', 25),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::X3),
            HashMap::from([
                ('a', 24),
                ('b', 24),
                ('c', 24),
                ('d', 24),
                ('e', 24),
                ('f', 24),
                ('g', 24),
                ('h', 24),
                ('+', 24),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::R1),
            HashMap::from([
                ('a', 26),
                ('b', 26),
                ('c', 26),
                ('d', 26),
                ('e', 26),
                ('f', 26),
                ('g', 26),
                ('h', 26),
                ('x', 26),
                (':', 26),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::R2),
            HashMap::from([
                ('1', 23),
                ('2', 23),
                ('3', 23),
                ('4', 23),
                ('5', 23),
                ('6', 23),
                ('7', 23),
                ('8', 23),
                ('+', 23),
                ('#', 23),
                ('x', 23),
                (':', 23),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::_S),
            HashMap::from([('-', 37), ('/', 37)]),
        ),
        (
            Token::NonTerminator(NonTerminator::_F),
            HashMap::from([
                ('a', 38),
                ('b', 38),
                ('c', 38),
                ('d', 38),
                ('e', 38),
                ('f', 38),
                ('g', 38),
                ('h', 38),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::_R),
            HashMap::from([
                ('1', 39),
                ('2', 39),
                ('3', 39),
                ('4', 39),
                ('5', 39),
                ('6', 39),
                ('7', 39),
                ('8', 39),
            ]),
        ),
        (
            Token::NonTerminator(NonTerminator::_B),
            HashMap::from([('O', 40), ('0', 40)]),
        ),
    ])
});

#[derive(Debug)]
pub enum SANError {
    SyntaxError,
    QueryError,
    InvalidPieceIdentifier,
    IncompleteNotation,
    UnrecognizedSymbol,
    ExpectedRank,
    ExpectedFile,
    ExpectedPiece,
    ExpectedModifier,
    ExpectedLocation,
    Uncastleable,
}

#[derive(Debug)]
pub enum Operation {
    Move(usize, Square),
    DrawOffer,
    Castle,
    Score(Color),
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
enum NonTerminator {
    M = 0,
    F,
    R,
    B,
    P,
    Phi,
    Tau,
    Kappa,
    X,
    Z,
    Lambda,
    Pi,
    B1,
    Phi1,
    Tau1,
    Kappa1,
    X1,
    B2,
    S,
    X2,
    F1,
    F2,
    F3,
    X3,
    R1,
    R2,
    _F,
    _R,
    _B,
    _S,
}

#[derive(Eq, Debug)]
enum Token {
    NonTerminator(NonTerminator),
    Terminator(char),
}

impl From<NonTerminator> for Token {
    fn from(non_terminator: NonTerminator) -> Self {
        Token::NonTerminator(non_terminator)
    }
}

impl From<char> for Token {
    fn from(terminator: char) -> Self {
        Token::Terminator(terminator)
    }
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Token::Terminator(t) => t.hash(state),
            Token::NonTerminator(nt) => (*nt as u32).hash(state),
        };
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Token::Terminator(t) => match other {
                Token::Terminator(other_t) => other_t == t,
                _ => false,
            },
            Token::NonTerminator(nt) => match other {
                Token::NonTerminator(other_nt) => *other_nt as usize == *nt as usize,
                _ => false,
            },
        }
    }
}

#[derive(Debug)]
pub enum NotationType {
    Score(Color),
    Move(MoveType),
}

#[derive(Debug)]
pub enum MoveType {
    Castle,
    Reloc(Profile),
}

#[derive(Debug)]
pub struct Profile {
    ty: PieceType,
    file_disambiguation: Option<u8>,
    rank_disambiguation: Option<u8>,
    target_square: Option<Square>,
}

#[derive(Debug)]
pub struct SAN {
    pub operation: Operation,
}

impl SAN {
    pub fn build(
        san_string: &str,
        positions: &Vec<Position>,
        active_color: &Color,
    ) -> SANResult<Self> {
        assert!(!san_string.is_empty());

        let (_, notation_type) = parse(san_string)?;

        match notation_type {
            NotationType::Move(ty) => match ty {
                MoveType::Reloc(subject_profile) => {
                    let mut active_color_positions = vec![];

                    for position in positions {
                        if position.piece.color == *active_color {
                            active_color_positions.push(position.clone());
                        }
                    }
                    let target_square = subject_profile.target_square;
                    let subject = query(subject_profile, &active_color_positions)?;
                    Ok(Self {
                        operation: Operation::Move(subject.id, target_square.unwrap()),
                    })
                }
                MoveType::Castle => {
                    if is_castleable(*active_color, positions) {
                        return Ok(Self {
                            operation: Operation::Castle,
                        });
                    }

                    Err(SANError::Uncastleable)
                }
            },
            NotationType::Score(winner) => Ok(Self {
                operation: Operation::Score(winner),
            }),
        }
    }
}

pub fn query(subject_profile: Profile, positions: &Vec<Position>) -> SANResult<Position> {
    let mut targets = vec![];

    for position in positions {
        if subject_profile.ty != position.piece.ty {
            continue;
        }

        if is_reachable(&subject_profile, position) {
            targets.push(position.clone());
        }
    }

    if targets.is_empty() {
        return Err(SANError::QueryError);
    }

    if targets.len() > 1 {
        return Err(SANError::QueryError);
    }

    Ok(targets[0].clone())
}

fn is_reachable(subject_profile: &Profile, position: &Position) -> bool {
    let target_square = subject_profile.target_square.unwrap();

    match subject_profile.ty {
        PieceType::Rook => {
            if (target_square.file == position.square.file)
                && (target_square.rank != position.square.rank)
            {
                return true;
            } else if (target_square.rank == position.square.rank)
                && (target_square.file != position.square.file)
            {
                return true;
            }

            false
        }
        PieceType::Knight => {
            let difference = (u8::abs_diff(target_square.file, position.square.file), u8::abs_diff(target_square.rank, position.square.rank));
            if difference.0 == 1 && difference.1 == 2 {
                return true;
            } else if difference.0 == 2 && difference.1 == 1 {
                return true;
            }

            false
        },
        PieceType::Pawn => {
            if target_square.file != position.square.file {
                return false;
            }

            let difference = u8::abs_diff(target_square.rank, position.square.rank);

            match position.piece.color {
                Color::Black => {
                    if target_square.rank > position.square.rank || difference > 1 {
                        return false;
                    }

                    true
                },
                Color::White => {
                    if target_square.rank < position.square.rank || difference > 1 {
                        return false;
                    }

                    true
                },
            }
        },
        PieceType::Queen => false,
        PieceType::King => {
            let difference = (u8::abs_diff(target_square.file, position.square.file), u8::abs_diff(target_square.rank, position.square.rank));
            if difference.0 > 1 || difference.1 > 1 {
                return false;
            }

            true
        }
        PieceType::Bishop => {
            if (target_square.file + position.square.rank)
                == (target_square.rank + position.square.file)
            {
                return true;
            }

            false
        }
    }
}

pub(crate) fn is_castleable(_active_color: Color, _positions: &[Position]) -> bool {
    true
}

/// LL(1) parser for Standard Algebraic Notation.
pub fn parse(san_string: &str) -> SANResult<(Vec<usize>, NotationType)> {
    if san_string.is_empty() {
        return Err(SANError::IncompleteNotation);
    }

    let mut stack: Vec<Token> = vec![NonTerminator::M.into()];
    let mut rules = vec![];
    let mut san_string = san_string.chars();
    let mut input_symbol = san_string.next();
    let mut profile = Profile {
        ty: PieceType::Pawn,
        target_square: None,
        file_disambiguation: None,
        rank_disambiguation: None,
    };
    let (mut f_ovw, mut r_ovw) = (false, false);

    while !stack.is_empty() {
        let top = stack.last().unwrap();

        match input_symbol {
            Some(symbol) => {
                if <char as Into<Token>>::into(symbol) == *top {
                    stack.pop();
                    input_symbol = san_string.next();
                    continue;
                }

                let rule = if let Some(entry) = PARSE_TABLE.get(top) {
                    entry.get(&symbol)
                } else {
                    return Err(SANError::SyntaxError);
                };

                stack.pop();

                if let Some(rule) = rule {
                    rules.push(*rule);

                    match rule {
                        1 => match symbol {
                            'a'..='h' => {
                                stack.push(NonTerminator::F.into());
                                profile.target_square =
                                    Some(Square::build(to_number(symbol), 1).unwrap());
                            }
                            'K' | 'Q' | 'N' | 'B' | 'R' => {
                                stack.push(NonTerminator::P.into());
                                profile.ty = symbol.try_into().unwrap();
                            }
                            'O' => stack.push(NonTerminator::B.into()),
                            '1' => {
                                stack.push(Token::Terminator('0'));
                                stack.push(Token::Terminator('-'));
                            }
                            '0' => stack.push(NonTerminator::Phi.into()),
                            '½' => stack.push(NonTerminator::Tau.into()),
                            '+' => {
                                stack.push(Token::Terminator('-'));
                                stack.push(Token::Terminator('/'));
                            }
                            '-' => stack.push(NonTerminator::Kappa.into()),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        2 => match symbol {
                            'a'..='h' => {
                                stack.push(NonTerminator::Lambda.into());
                                profile.file_disambiguation = Some(to_number(symbol));
                            }
                            '1'..='8' => {
                                stack.push(NonTerminator::R.into());
                                profile.target_square =
                                    Some(Square::build(profile.target_square.unwrap().file, to_number_r(symbol))
                                        .unwrap());
                            }
                            'x' | ':' => stack.push(NonTerminator::X.into()),
                            _ => return Err(SANError::ExpectedLocation),
                        },
                        3 => match symbol {
                            'a'..='h' => stack.push(NonTerminator::Pi.into()),
                            '=' | '/' => stack.push(NonTerminator::Z.into()),
                            'x' | ':' => stack.push(NonTerminator::X2.into()),
                            '+' | '#' => (),
                            'K' | 'Q' | 'N' | 'B' | 'R' => stack.push(NonTerminator::Lambda.into()),
                            _ => return Err(SANError::ExpectedModifier),
                        },
                        4 => match symbol {
                            'a'..='h' => {
                                stack.push(NonTerminator::F2.into());
                                profile.target_square =
                                    Some(Square::build(to_number(symbol), 1).unwrap());
                                f_ovw = true;
                            }
                            '1'..='8' => {
                                stack.push(NonTerminator::R1.into());
                                
                                if f_ovw {
                                    profile.target_square =
                                        Some(Square::build(profile.target_square.unwrap().file, to_number_r(symbol))
                                            .unwrap());
                                }

                            }
                            'x' | ':' => stack.push(NonTerminator::X1.into()),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        5 => match symbol {
                            '-' => stack.push(NonTerminator::B1.into()),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        6 => match symbol {
                            '-' => stack.push(NonTerminator::Phi1.into()),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        7 => match symbol {
                            '-' => stack.push(NonTerminator::Tau1.into()),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        8 => match symbol {
                            '/' | '-' => stack.push(NonTerminator::Kappa1.into()),
                            _ => return Err(SANError::ExpectedModifier),
                        },
                        9 => match symbol {
                            'a'..='h' => stack.push(NonTerminator::F1.into()),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        10 => match symbol {
                            'K' | 'Q' | 'N' | 'B' | 'R' => stack.push(NonTerminator::Lambda.into()),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        11 => match symbol {
                            '+' | '#' => (),
                            _ => return Err(SANError::ExpectedModifier),
                        },
                        12 => match symbol {
                            'O' => stack.push(NonTerminator::B2.into()),
                            _ => return Err(SANError::ExpectedModifier),
                        },
                        13 => match symbol {
                            '1' | '0' | '½' => (),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        14 => match symbol {
                            '0' | '½' => (),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        15 => match symbol {
                            '+' | '-' => (),
                            _ => return Err(SANError::ExpectedModifier),
                        },
                        16 => match symbol {
                            'a'..='h' => {
                                stack.push(NonTerminator::Pi.into());
                                profile.target_square =
                                    Some(Square::build(to_number(symbol), 1).unwrap());
                            }
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        17 => match symbol {
                            '-' => stack.push(NonTerminator::S.into()),
                            '+' | '#' => (),
                            _ => return Err(SANError::ExpectedModifier),
                        },
                        18 => match symbol {
                            'O' => stack.push(NonTerminator::Lambda.into()),
                            _ => return Err(SANError::ExpectedModifier),
                        },
                        19 => match symbol {
                            'a'..='h' => stack.push(NonTerminator::Pi.into()),
                            '1'..='8' => {
                                stack.push(Token::Terminator('#'));
                                stack.push(NonTerminator::_R.into());
                                stack.push(NonTerminator::_F.into());
                            }
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        20 => match symbol {
                            '1'..='8' => {
                                stack.push(NonTerminator::Lambda.into());
                                if !r_ovw {
                                    profile.rank_disambiguation = Some(profile.target_square.unwrap().rank);
                                }

                                    profile.target_square =
                                        Some(Square::build(profile.target_square.unwrap().file, to_number_r(symbol))
                                            .unwrap());
                            }
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        21 => match symbol {
                            '1'..='8' => stack.push(NonTerminator::Lambda.into()),
                            '+' | '#' => (),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        22 => match symbol {
                            '1'..='8' => {
                                stack.push(NonTerminator::R2.into());

                                profile.target_square =
                                    Some(Square::build(profile.target_square.unwrap().file, to_number_r(symbol))
                                        .unwrap());
                                r_ovw = true;
                            }
                            'a'..='h' => {
                                stack.push(NonTerminator::Pi.into());

                                profile.file_disambiguation = Some(to_number(symbol));
                            }
                            _ => return Err(SANError::ExpectedRank),
                        },
                        23 => {
                            match symbol {
                                'x' | ':' => stack.push(NonTerminator::X3.into()),
                                '+' => (),
                                '#' => (),
                                '1'..='8' => {
                                    stack.push(NonTerminator::Lambda.into());
                                }
                                _ => return Err(SANError::UnrecognizedSymbol),
                            }
                        },
                        24 => match symbol {
                            'a'..='h' => {
                                stack.push(NonTerminator::Pi.into());
                                if f_ovw {
                                    profile.file_disambiguation = Some(profile.target_square.unwrap().file);
                                }

                                profile.target_square =
                                    Some(Square::build(to_number(symbol), 1)
                                        .unwrap());
                            }
                            '+' => (),
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        25 => match symbol {
                            '1'..='8' => {
                                stack.push(NonTerminator::R2.into());
                                profile.rank_disambiguation = Some(to_number_r(symbol));
                            }
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        26 => match symbol {
                            'K' | 'Q' | 'N' | 'B' | 'R' => (),
                            'a'..='h' => {
                                stack.push(NonTerminator::F3.into());
                                profile.file_disambiguation = Some(to_number(symbol));
                            }
                            _ => return Err(SANError::UnrecognizedSymbol),
                        },
                        38 => match symbol {
                            'a'..='h' => (),
                            _ => return Err(SANError::ExpectedFile),
                        }
                        39 => match symbol {
                            '1'..='8' => (),
                            _ => return Err(SANError::ExpectedRank),
                        }
                        other => panic!("rule {other} is not yet implemented"),
                    }

                    stack.push(Token::Terminator(symbol));
                } else {
                    return Err(SANError::SyntaxError);
                }
            }
            _ => {
                match *top {
                    Token::NonTerminator(NonTerminator::Lambda)
                        | Token::NonTerminator(NonTerminator::R2)
                        | Token::NonTerminator(NonTerminator::X3)
                        | Token::NonTerminator(NonTerminator::F1)
                        | Token::NonTerminator(NonTerminator::B2)
                        | Token::NonTerminator(NonTerminator::R) => break,
                        _ => return Err(SANError::IncompleteNotation),
                }
            },
        }
    }

    let mut left_derivations = rules.clone();
    let _notation_type = NotationType::Move(MoveType::Castle);
    let move_type = MoveType::Reloc(profile);

    let deriv = left_derivations.pop();

    let notation_type = match deriv {
        Some(2) | Some(4) | Some(5) | Some(11) | Some(20) | Some(18) | Some(24) | Some(39) | Some(38) | Some(14) | Some(22) | Some(3) | Some(23) | Some(25) | Some(12) | Some(10) => {
            NotationType::Move(move_type)
        }
        Some(6) => NotationType::Score(Color::Black),
        Some(8) => NotationType::Score(Color::Black),
        Some(13) => NotationType::Score(Color::Black),
        Some(15) => NotationType::Score(Color::Black),
        Some(1) => NotationType::Score(Color::White),
        Some(e) => panic!("please implement {e}"),
        None => panic!(),
    };

    Ok((rules, notation_type))
}

fn to_number(c: char) -> u8 {
    c as u8 - 96
}

fn to_number_r(c: char) -> u8 {
    c as u8 - 48
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter;
    use crate::{position, Piece};

    #[test]
    fn parse_known_valid_moves() {
        let san_strings = vec![
            "Nf3", "e4", "Rdf8", "Qh4xe1#", "e8Q", "Rf6+", "Bc4:", "1-0", "+/-", "-/+", "O-O-O+",
            "Rb8#", "e5d6", "Rd3xd7", "0-1", "0-0",
        ];
        let rules = vec![
            vec![1, 4, 22],
            vec![1, 2],
            vec![1, 4, 22, 20],
            vec![1, 4, 22, 23, 24, 20, 11],
            vec![1, 2, 3],
            vec![1, 4, 22, 23],
            vec![1, 4, 22, 23],
            vec![1],
            vec![1],
            vec![1, 8, 15],
            vec![1, 5, 12, 17, 18, 11],
            vec![1, 4, 22, 23],
            vec![1, 2, 3, 20],
            vec![1, 4, 22, 23, 24, 20],
            vec![1, 6, 13],
            vec![1, 6, 13],
        ];

        for (san_string, rule) in iter::zip(san_strings, rules) {
            assert_eq!(parse(san_string).unwrap().0, rule);
        }
    }

    #[test]
    #[should_panic]
    fn parse_known_incomplete_moves() {
        let san_strings = vec![
            "Nf", "e", "Rdf"
        ];

        for san_string in san_strings {
            let _ = parse(san_string).unwrap();
        }
    }

    #[test]
    fn find_correct_piece() {
        let san_strings = vec!["Rg6", "Ke5", "Ne2"];
        let positions = vec![
            position!(7, 1, PieceType::Knight, Color::White),
            position!(7, 5, PieceType::Rook, Color::White),
            position!(6, 4, PieceType::King, Color::White),
        ];
        let expected_positions = vec![&positions[1], &positions[2], &positions[0]];
        let mut chess_moves = vec![];

        for (expected_position, san_string) in iter::zip(expected_positions.clone(), san_strings) {
            let chess_move =
                SAN::build(san_string, &positions, &expected_position.piece.color).unwrap();
            chess_moves.push(chess_move);
        }

        for (expected_position, chess_move) in iter::zip(expected_positions, chess_moves) {
            match chess_move.operation {
                Operation::Move(position_id, target_square) => {
                    dbg!(positions
                        .iter().find(|p| p.id == position_id)
                        .expect("Expected valid position id"));
                    assert_eq!(position_id, expected_position.id);
                }
                _ => panic!(),
            }
        }
    }
}
