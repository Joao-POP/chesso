use crate::{position, Color, Piece, PieceType, Position, PositionError, Square};

pub use std::cmp::Ordering;

type FENResult<T> = Result<T, FENError>;

#[derive(Debug)]
pub enum FENError {
    BadlyFormattedField(FENField),
    TooManyPieces(usize),
    TooLittlePieces(usize),
    EmptyRank,
    FileGeometryFailed(u8),
    RankGeometryFailed(u8),
    IncorrectFieldCount,
    InvalidColor,
}

impl From<PositionError> for FENError {
    fn from(error: PositionError) -> Self {
        match error {
            PositionError::FileOutOfBounds(c) => FENError::FileGeometryFailed(c),
            PositionError::RankOutOfBounds(r) => FENError::RankGeometryFailed(r),
        }
    }
}

#[derive(Debug)]
#[allow(unused)]
enum FENField {
    PiecePlacement,
    ActiveColor,
    CastlingAvailability,
    EnPassanTargetSquare,
    HalfmoveClock,
    FullmoveNumber,
}

pub struct FENData<'a> {
    piece_placement_data: &'a str,
    active_color_field: char,
    castling_availability: &'a str,
    en_passant_target_square: &'a str,
    fullmove_number_field: &'a str,
    halfmove_clock_field: &'a str,
}

impl<'a> FENData<'a> {
    pub fn build(fen_string: &'a str) -> FENResult<Self> {
        assert!(!fen_string.is_empty());

        let fields: Vec<_> = fen_string.split_whitespace().collect();

        if fields.len() != 6 {
            return Err(FENError::IncorrectFieldCount);
        }

        if fields[1].len() != 1 {
            return Err(FENError::BadlyFormattedField(FENField::ActiveColor));
        }

        let active_color_field = fields[1].chars().next().unwrap();

        Ok(Self {
            piece_placement_data: fields[0],
            active_color_field,
            castling_availability: fields[2],
            en_passant_target_square: fields[3],
            halfmove_clock_field: fields[4],
            fullmove_number_field: fields[5],
        })
    }

    pub fn generate_positions(&self) -> FENResult<Vec<Position>> {
        assert!(!self.piece_placement_data.is_empty());

        let mut positions = vec![];
        let mut skipped_square_count: usize = 0;

        for (rank, pieces) in self.piece_placement_data.split('/').enumerate() {
            if pieces.is_empty() {
                return Err(FENError::EmptyRank);
            }

            let mut file_advance = 0;

            for (col, piece) in pieces.chars().enumerate() {
                let (piecety, color) = match piece {
                    'p' => (PieceType::Pawn, Color::Black),
                    'P' => (PieceType::Pawn, Color::White),
                    'n' => (PieceType::Knight, Color::Black),
                    'N' => (PieceType::Knight, Color::White),
                    'r' => (PieceType::Rook, Color::Black),
                    'R' => (PieceType::Rook, Color::White),
                    'q' => (PieceType::Queen, Color::Black),
                    'Q' => (PieceType::Queen, Color::White),
                    'k' => (PieceType::King, Color::Black),
                    'K' => (PieceType::King, Color::White),
                    'b' => (PieceType::Bishop, Color::Black),
                    'B' => (PieceType::Bishop, Color::White),
                    '1'..='8' => {
                        let skipped = char::to_digit(piece, 10).unwrap() as usize;

                        skipped_square_count += skipped;
                        file_advance += skipped - 1;

                        continue;
                    }
                    _ => continue,
                };

                let file = 1 + col as u8 + file_advance as u8;

                let position = Position::new(
                    Square::build(file, 8 - rank as u8)?,
                    Piece::new(piecety, color),
                );

                positions.push(position);
            }
        }

        let processed_piece_count = positions.len() + skipped_square_count;

        match processed_piece_count.cmp(&64) {
            Ordering::Greater => Err(FENError::TooManyPieces(processed_piece_count)),
            Ordering::Less => Err(FENError::TooLittlePieces(processed_piece_count)),
            Ordering::Equal => Ok(positions),
        }
    }

    pub fn get_active_color(&self) -> FENResult<Color> {
        match self.active_color_field {
            'w' => Ok(Color::White),
            'b' => Ok(Color::Black),
            _ => Err(FENError::InvalidColor),
        }
    }

    pub fn get_castling_availability(&self) -> FENResult<()> {
        todo!()
    }

    pub fn get_en_passant_target_square(&self) -> FENResult<u8> {
        todo!()
    }

    pub fn get_fullmove_number(&self) -> FENResult<u8> {
        Ok(self.fullmove_number_field.parse::<u8>().unwrap())
    }

    pub fn get_halfmove_clock(&self) -> FENResult<u8> {
        Ok(self.halfmove_clock_field.parse::<u8>().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn closest_match(query: &Position, list: &Vec<Position>) -> Position {
        let mut matches = vec![];

        for position in list {
            let mut score: i32 = 0;

            // criteria: piece > file > rank > color
            if *query == *position {
                return query.clone();
            }

            if query.piece.ty == position.piece.ty {
                score += 13;
            }

            score -= (query.square.file as i32).abs_diff(position.square.file.into()) as i32;
            score -= (query.square.rank as i32).abs_diff(position.square.rank.into()) as i32;

            if query.piece.color == position.piece.color {
                score += 1;
            }

            matches.push((score, position));
        }

        matches.sort_by(|a, b| b.0.cmp(&a.0));

        matches[0].1.clone()
    }

    #[test]
    fn test_closest_match() {
        let positions = vec![
            position!(1, 8, PieceType::Rook, Color::Black),
            position!(4, 3, PieceType::Bishop, Color::Black),
            position!(2, 5, PieceType::Pawn, Color::White),
            position!(5, 5, PieceType::Pawn, Color::Black),
            position!(7, 5, PieceType::Pawn, Color::Black),
            position!(7, 2, PieceType::Queen, Color::Black),
            position!(1, 5, PieceType::Rook, Color::White),
            position!(7, 8, PieceType::Knight, Color::White),
        ];

        let approx_positions = vec![
            position!(3, 8, PieceType::Rook, Color::White),
            position!(4, 3, PieceType::Bishop, Color::Black),
            position!(2, 5, PieceType::Pawn, Color::Black),
            position!(6, 5, PieceType::Pawn, Color::Black),
            position!(7, 5, PieceType::Pawn, Color::Black),
            position!(8, 6, PieceType::Queen, Color::Black),
            position!(1, 4, PieceType::Rook, Color::White),
            position!(7, 8, PieceType::Knight, Color::Black),
        ];

        assert_eq!(
            closest_match(&approx_positions[0], &positions),
            positions[0]
        );
        assert_eq!(
            closest_match(&approx_positions[1], &positions),
            positions[1]
        );
        assert_eq!(
            closest_match(&approx_positions[2], &positions),
            positions[2]
        );
        assert_eq!(
            closest_match(&approx_positions[3], &positions),
            positions[3]
        );
        assert_eq!(
            closest_match(&approx_positions[4], &positions),
            positions[4]
        );
    }

    #[test]
    fn generate_positions_single_pawn() {
        let rank_field_string = "p7/8/8/8/8/8/8/8 w KQkq - 0 1";
        let expected_piece = position!(1, 8, PieceType::Pawn, Color::Black);
        let fen_data = FENData::build(rank_field_string).unwrap();
        let positions = fen_data.generate_positions().unwrap();

        assert_eq!(closest_match(&expected_piece, &positions), expected_piece);
    }

    #[test]
    fn generate_positions() {
        let rank_field_string = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 1";
        let expected_positions = vec![
            position!(1, 8, PieceType::Rook, Color::Black),
            position!(8, 8, PieceType::Rook, Color::Black),
            position!(1, 1, PieceType::Rook, Color::White),
            position!(8, 1, PieceType::Rook, Color::White),
            position!(3, 5, PieceType::Pawn, Color::Black),
            position!(5, 4, PieceType::Pawn, Color::White),
            position!(6, 3, PieceType::Knight, Color::White),
            position!(1, 2, PieceType::Pawn, Color::White),
            position!(8, 2, PieceType::Pawn, Color::White),
        ];
        let fen_data = FENData::build(rank_field_string).unwrap();
        let positions = fen_data.generate_positions().unwrap();

        dbg!(&positions);

        for expected_position in expected_positions {
            assert_eq!(
                closest_match(&expected_position, &positions),
                expected_position
            );
        }
    }

    #[test]
    fn generate_positions_mostly_empty() {
        let rank_field_string = "p5p1/6p1/3pP3/PPPP4/p5p1/6p1/3pP3/PPPP4 w KQkq - 0 1";
        let fen_data = FENData::build(rank_field_string).unwrap();
        let positions = fen_data.generate_positions().unwrap();
        let expected_positions = vec![
            position!(1, 8, PieceType::Pawn, Color::Black),
            position!(3, 5, PieceType::Pawn, Color::White),
            position!(4, 2, PieceType::Pawn, Color::Black),
        ];

        dbg!(&positions);

        for expected_position in expected_positions {
            assert_eq!(
                closest_match(&expected_position, &positions),
                expected_position
            );
        }

        assert_eq!(positions.len(), 18);
    }

    #[test]
    fn get_active_color() {
        let fen_string = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let fen_data = FENData::build(fen_string).unwrap();

        assert_eq!(fen_data.get_active_color().unwrap(), Color::White);
    }

    #[test]
    fn get_halfmove_clock() {
        let fen_string = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let fen_data = FENData::build(fen_string).unwrap();

        assert_eq!(fen_data.get_halfmove_clock().unwrap(), 0);
    }

    #[test]
    fn get_fullmove_number() {
        let fen_string = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let fen_data = FENData::build(fen_string).unwrap();

        assert_eq!(fen_data.get_fullmove_number().unwrap(), 1);
    }
}
