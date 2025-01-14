use chesso::{Board, BoardError, DrawCondition, Reason};
use std::{fs, io};

fn main() {
    let initial_position = fs::read_to_string("games/standard.fen").unwrap();
    let mut board = match Board::build(&initial_position) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Couldn't create board due to error: {e:?}");
            return;
        }
    };
    
    println!("{}", board);

    loop {
        let mut user_input = String::new();

        io::stdin().read_line(&mut user_input).unwrap();

        if !user_input.trim().is_empty() {

            let feedback = board.execute_move(&user_input);

            match feedback {
                Ok(result) =>
                    if let Some(winner) = result {
                    println!("{winner:?} wins");
                    return;
                },
                Err(BoardError::IllegalMove(reason)) => match reason {
                    Reason::DiscoveryCheck => println!("This move is blocked because it would uncover the king from an attacking piece."),
                    Reason::Unreachable => println!("This piece cannot make this movement pattern."),
                    Reason::BlockedByAlly => println!("This would capture your own ally."),
                    Reason::IgnoreCheck => println!("You cannot ignore a check."),
                },
                Err(BoardError::MalformedNotation) => eprintln!("Please, input standard algebraic notation."),
                Err(BoardError::Draw(condition)) => match condition {
                    DrawCondition::Stalemate => {
                        println!("You have stalemated yourself.");
                        return;
                    },
                    DrawCondition::MaxMoves => {
                        println!("Maximum number of moves reached. This is a draw.");
                        return;
                    }
                },
                Err(BoardError::SANError(san_error)) => eprintln!("SAN Error: {san_error:?}"),
                Err(_) => (),
            }
        }

        println!("{}", board);
    }
}
