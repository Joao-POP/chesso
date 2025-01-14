pub mod fen;
pub mod pgn;
pub mod san;

use fen::{FENData, FENError};
use san::{Operation, SAN};
use std::fmt;

pub use san::SANError;
pub use san::parse;

#[doc(hidden)]
#[macro_export]
macro_rules! position {
    ($file:expr, $rank:expr, $piece_type:expr, $color:expr) => {
        Position::new(
            Square::build($file, $rank).unwrap(),
            Piece::new($piece_type, $color),
        )
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! standard_board {
    () => {
        let standard_board_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        Board::build(standard_board_fen).unwrap()
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! white_king {
    () => {
        Position::new(
            Square::build(5, 1).unwrap(),
            Piece::new(PieceType::King, Color::White),
        )
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! black_king {
    () => {
        Position::new(
            Square::build(5, 8).unwrap(),
            Piece::new(PieceType::King, Color::Black),
        )
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! square {
    ($file:expr, $rank:expr) => {
        Square::build($file, $rank).unwrap()
    };
}

#[derive(Debug)]
pub enum BoardError {
    IllegalMove(Reason),
    MalformedNotation,
    Draw(DrawCondition),
    SANError(SANError),
    PositionError(PositionError),
    FENError(FENError),
}

impl From<SANError> for BoardError {
    fn from(error: SANError) -> Self {
        Self::SANError(error)
    }
}

impl From<PositionError> for BoardError {
    fn from(error: PositionError) -> Self {
        Self::PositionError(error)
    }
}

impl From<FENError> for BoardError {
    fn from(error: FENError) -> Self {
        Self::FENError(error)
    }
}

#[derive(Debug)]
pub enum Reason {
    Unreachable,
    BlockedByAlly,
    DiscoveryCheck,
    IgnoreCheck,
}

#[derive(Debug)]
pub enum DrawCondition {
    Stalemate,
    MaxMoves,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Color {
    Black,
    White,
}

#[derive(Debug)]
pub enum PositionError {
    FileOutOfBounds(u8),
    RankOutOfBounds(u8),
}

/// A list of all piece types in chess.
#[derive(Debug, PartialEq, Clone)]
enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl TryFrom<char> for PieceType {
    type Error = SANError;

    fn try_from(san_identifier: char) -> Result<Self, Self::Error> {
        match san_identifier {
            'K' => Ok(PieceType::King),
            'Q' => Ok(PieceType::Queen),
            'R' => Ok(PieceType::Rook),
            'B' => Ok(PieceType::Bishop),
            'N' => Ok(PieceType::Knight),
            other => Err(SANError::InvalidPieceIdentifier),
        }
    }
}

/// A board with the state of a game of chess.
///
/// This includes the pieces on the board, the clocks, the turn and other
/// configuration. It may be created with either FEN strings or PGN data.
///
/// # Example
/// ```
/// use chesso::{Board, BoardError};
///
/// # fn main() -> Result<(), BoardError> {
/// let two_pawns_game = "8/8/8/p7/7P/8/8/8 w KQkq - 0 1";
/// let mut board = Board::build(two_pawns_game)?;
///
/// board.execute_move("h5")?;
/// Ok(())
/// # }
/// ```
pub struct Board {
    positions: Vec<Position>,
    active_color: Color,
    halfmove_clock: u8,
    fullmove_number: u8,
}

impl Board {
    pub fn build(fen_string: &str) -> Result<Self, FENError> {
        let fen_data = FENData::build(fen_string)?;

        let positions = fen_data.generate_positions()?;
        let active_color = fen_data.get_active_color()?;
        let halfmove_clock = fen_data.get_halfmove_clock()?;
        let fullmove_number = fen_data.get_fullmove_number()?;

        Ok(Self {
            positions,
            active_color,
            halfmove_clock,
            fullmove_number,
        })
    }

    pub fn execute_move(&mut self, san_string: &str) -> Result<Option<Color>, BoardError> {
        let san = SAN::build(san_string.trim(), &self.positions, &self.active_color)?;

        match san.operation {
            Operation::Castle => (),
            Operation::Move(subject_id, target_square) => {
                self.move_piece(subject_id, target_square);
            }
            Operation::DrawOffer => (),
            Operation::Score(_) => (),
        }

        Ok(self.analyze_board_for_checkmate())
    }

    fn analyze_board_for_checkmate(&self) -> Option<Color> {
        None
    }

    fn move_piece(&mut self, position_id: usize, target: Square) {
        for position in &mut self.positions {
            if position.id == position_id {
                position.square = target;
            }
        }

        match self.active_color {
            Color::Black => self.active_color = Color::White,
            Color::White => self.active_color = Color::Black,
        }
    }
}

impl fmt::Display for Board {
    #[allow(unused_must_use)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let escape_code = char::from_u32(27).unwrap();

        match self.active_color {
            Color::White => {
                for rank in 1..=8 {
                    for file in 1..=8 {
                        if (rank + file) % 2 == 1 {
                            write!(f, "{CSI}[7m  {CSI}[0m", CSI = escape_code);
                        } else {
                            write!(f, "  ");
                        }

                        if file % 8 == 0 {
                            write!(f, "\n");
                        }
                    }
                }

                write!(f, "{CSI}s", CSI = escape_code);

                for position in &self.positions {
                    if (position.square.file + position.square.rank) % 2 == 0 {
                        write!(f, "{CSI}[7m", CSI = escape_code);
                    } else {
                        write!(f, "{CSI}[0m", CSI = escape_code);
                    }

                    write!(
                        f,
                        "{CSI}[{rank}A{CSI}[{file}G{icon}{CSI}[{rank}B{CSI}u{CSI}[0m",
                        CSI = escape_code,
                        rank = position.square.rank,
                        file = position.square.file * 2 - 1,
                        icon = position.piece.to_string().chars().next().unwrap(),
                    );
                }

                Ok(())
            },
            Color::Black => {
                for rank in 1..=8 {
                    for file in 1..=8 {
                        if (rank + file) % 2 == 0 {
                            write!(f, "{CSI}[7m  {CSI}[0m", CSI = escape_code);
                        } else {
                            write!(f, "  ");
                        }

                        if file % 8 == 0 {
                            write!(f, "\n");
                        }
                    }
                }

                write!(f, "{CSI}s", CSI = escape_code);

                for position in &self.positions {
                    if (position.square.file + position.square.rank) % 2 == 0 {
                        write!(f, "{CSI}[7m", CSI = escape_code);
                    } else {
                        write!(f, "{CSI}[0m", CSI = escape_code);
                    }

                    write!(
                        f,
                        "{CSI}[{rank}A{CSI}[{file}G{icon}{CSI}[{rank}B{CSI}u{CSI}[0m",
                        CSI = escape_code,
                        rank = 9 - position.square.rank,
                        file = (position.square.file * 2 - 1),
                        icon = position.piece.to_string().chars().next().unwrap(),
                    );
                }

                Ok(())
            }
        }
    }
}

/// An actual piece on a chess board.
///
/// Is guaranteed to be inside bounds thanks to `crate::Square`.
#[derive(Debug, Clone)]
struct Position {
    square: Square,
    piece: Piece,
    id: usize,
}

impl Position {
    pub fn new(square: Square, piece: Piece) -> Self {
        let id = rand::random::<usize>();

        Self { square, piece, id }
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        self.square == other.square && self.piece == other.piece && self.piece.ty == other.piece.ty
    }
}

/// A chess piece.
///
/// May be any of the standard pieces (Knight, Rook, Queen, King, Pawn, Bishop),
/// and a color.
#[derive(Debug, Clone)]
struct Piece {
    color: Color,
    ty: PieceType,
}

impl Piece {
    fn new(ty: PieceType, color: Color) -> Self {
        Self { ty, color }
    }
}

impl PartialEq for Piece {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let icon = match self.color {
            Color::White => match self.ty {
                PieceType::Pawn => '♙',
                PieceType::Rook => '♖',
                PieceType::Knight => '♘',
                PieceType::Bishop => '♗',
                PieceType::Queen => '♕',
                PieceType::King => '♔',
            },
            Color::Black => match self.ty {
                PieceType::Pawn => '♟',
                PieceType::Rook => '♜',
                PieceType::Knight => '♞',
                PieceType::Bishop => '♝',
                PieceType::Queen => '♛',
                PieceType::King => '♚',
            },
        };

        write!(f, "{}", icon)
    }
}

/// A coordinate guaranteed to be inside the bounds of a chess board.
///
/// Use this if you need to represent any kind of location with just numbers.
#[derive(PartialEq, Debug, Copy, Clone)]
struct Square {
    file: u8,
    rank: u8,
}

impl Square {
    fn build(file: u8, rank: u8) -> Result<Self, PositionError> {
        if file > 8 || file < 1 {
            return Err(PositionError::FileOutOfBounds(file));
        } else if rank > 8 || rank < 1 {
            return Err(PositionError::RankOutOfBounds(rank));
        }

        Ok(Self { file, rank })
    }
}
