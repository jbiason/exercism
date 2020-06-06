#[deny(warnings, missing_docs)]

/// Errors created when in the game
#[derive(Debug, PartialEq)]
pub enum Error {
    /// The throw is higher the number of pins available.
    NotEnoughPinsLeft,

    /// The game is over, no more throws are possible.
    GameComplete,
}

/// A throw.
// I'd usually move those to their own file, but since Exercism wants a single file... here we go.
mod throw {
    #[deny(warnings, missing_docs)]
    use std::fmt;

    #[derive(PartialEq)]
    enum ThrowType {
        /// This throw doesn't exist yet; the player did something (a strike, a spare) that gave
        /// them another throw.
        Free,

        /// Pins knocked (or not, if the value is 0).
        Knock(u16),
    }

    pub struct Throw {
        /// The throw id; it is used only for our debugging reference.
        id: usize,

        /// The result of the throw.
        result: ThrowType,
    }

    impl Throw {
        /// Create a free Throw
        pub fn new_free(id: usize) -> Self {
            Self {
                id: id,
                result: ThrowType::Free,
            }
        }

        /// Create a throw that knocked some pins
        pub fn new(id: usize, pins: u16) -> Self {
            Self {
                id: id,
                result: ThrowType::Knock(pins),
            }
        }

        /// In-place update of a free throw
        pub fn update(&mut self, pins: u16) {
            // Silently refusing to update a non-free throw result.
            if self.result == ThrowType::Free {
                self.result = ThrowType::Knock(pins);
            }
        }

        /// Indicate that the Throw is a free and has no value.
        pub fn is_free_throw(&self) -> bool {
            self.result == ThrowType::Free
        }

        /// Indicate that the player made a strike with this throw.
        pub fn is_strike(&self) -> bool {
            self.result == ThrowType::Knock(10)
        }

        /// Score of this throw.
        pub fn score(&self) -> Option<u16> {
            match self.result {
                ThrowType::Free => None,
                ThrowType::Knock(x) => Some(x),
            }
        }
    }

    /// A different debug information, so it will be easier to read.
    impl fmt::Debug for Throw {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{}-{}",
                self.id,
                match self.result {
                    ThrowType::Free => "Free".into(),
                    ThrowType::Knock(x) => format!("{}", x),
                }
            )
        }
    }

    #[cfg(test)]
    mod throw_tests {
        #[test]
        fn make_free() {
            let throw = super::Throw::new_free(1);
            assert!(throw.is_free_throw());
            assert_eq!("1-Free", format!("{:?}", throw));
        }

        #[test]
        fn make_strike() {
            let throw = super::Throw::new(1, 10);
            assert!(!throw.is_free_throw());
            assert_eq!("1-10", format!("{:?}", throw));
            assert!(throw.is_strike());
        }

        #[test]
        fn update() {
            let mut throw = super::Throw::new_free(1);
            assert_eq!(throw.score(), None);
            throw.update(5);
            assert_eq!(throw.score(), Some(5));
        }
    }
}

/// A game frame
mod frame {
    #[deny(warnings, missing_docs)]
    use std::fmt;
    use std::rc::Rc;

    use super::throw::Throw;

    pub struct Frame {
        id: usize,
        throws: Vec<Rc<Throw>>,
    }

    impl Frame {
        pub fn new(id: usize) -> Self {
            Self {
                id: id,
                throws: vec![],
            }
        }

        pub fn add_throw(&mut self, throw: &Rc<Throw>) {
            self.throws.push(throw.clone());
        }

        pub fn possible_spare(&self, pins: u16) -> bool {
            self.throws.len() == 1 && self.score().unwrap() + pins == 10
        }

        pub fn score(&self) -> Option<u16> {
            if self.throws.len() == 0 || self.throws.iter().any(|throw| throw.is_free_throw()) {
                None
            } else {
                Some(self.throws.iter().map(|throw| throw.score().unwrap()).sum())
            }
        }

        pub fn is_closed(&self) -> bool {
            self.throws.len() >= 2 && self.throws.iter().all(|throw| !throw.is_free_throw())
        }
    }

    /// A different debug information, so it will be easier to read.
    impl fmt::Debug for Frame {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{}-{} ({})",
                self.id,
                match self.score() {
                    None => "??".into(),
                    Some(x) => format!("{}", x),
                },
                self.throws
                    .iter()
                    .map(|x| format!("{:?}", x))
                    .collect::<Vec<String>>()
                    .join(",")
            )
        }
    }

    #[cfg(test)]
    mod frame_tests {
        #[test]
        fn no_score() {
            let frame = super::Frame::new(1);
            assert_eq!(frame.score(), None);
        }

        #[test]
        fn score() {
            let mut frame = super::Frame::new(1);
            let throw = super::Throw::new(1, 10);
            frame.add_throw(&super::Rc::new(throw));
            assert_eq!(frame.score(), Some(10));
        }

        #[test]
        fn no_score_with_free_throws() {
            let mut frame = super::Frame::new(1);

            let throw1 = super::Throw::new(1, 5);
            let throw2 = super::Throw::new_free(2);

            frame.add_throw(&super::Rc::new(throw1));
            frame.add_throw(&super::Rc::new(throw2));

            assert_eq!(frame.score(), None);
        }

        #[test]
        fn debug() {
            let mut frame = super::Frame::new(1);

            let throw1 = super::Throw::new(1, 5);
            let throw2 = super::Throw::new_free(2);

            frame.add_throw(&super::Rc::new(throw1));
            frame.add_throw(&super::Rc::new(throw2));

            assert_eq!("1-?? (1-5,2-Free)", format!("{:?}", frame));
        }
    }
}

use std::rc::Rc;

pub struct BowlingGame {
    pub frames: Vec<frame::Frame>,
    pub throws: Vec<Rc<throw::Throw>>,
}

impl BowlingGame {
    pub fn new() -> Self {
        Self {
            frames: vec![],
            throws: vec![],
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if !self.has_free_throws() && self.is_finished() {
            Err(Error::GameComplete)
        } else if pins > 10 {
            Err(Error::NotEnoughPinsLeft)
        } else {
            let mut frame_throws: Vec<Rc<throw::Throw>> = Vec::new();
            if !self.has_free_throws() {
                let throw = self.add_new_throw(pins);
                frame_throws.push(throw);
            } else {
                self.reuse_free_throw(pins);
            }

            if pins == 10 {
                // On a strike, the player get two "free" throws.
                let last_id = self.throws.len();
                frame_throws.push(Rc::new(throw::Throw::new_free(last_id + 1)));
                frame_throws.push(Rc::new(throw::Throw::new_free(last_id + 2)));
            } else if self.spare(pins) {
                let last_id = self.throws.len();
                frame_throws.push(Rc::new(throw::Throw::new_free(last_id + 1)));
            }

            self.push_throws_to_last_frame(frame_throws.as_slice());
            self.push_free_throws(frame_throws.as_slice());

            dbg!(&self.throws);
            dbg!(&self.frames);

            Ok(())
        }
    }

    /// Add a new throw to the throw list.
    pub fn add_new_throw(&mut self, pins: u16) -> Rc<throw::Throw> {
        let last_id = self.throws.len();
        let new_throw = Rc::new(throw::Throw::new(last_id, pins));
        self.throws.push(new_throw.clone());
        new_throw.clone()
    }

    /// Reuse on the free throws in the throw list instead of creating a new one.
    pub fn reuse_free_throw(&mut self, pins: u16) {
        (*Rc::get_mut(
            self.throws
                .iter_mut()
                .filter(|throw| throw.is_free_throw())
                .take(1)
                .next()
                .expect("There are no free throws for reuse"),
        )
        .expect("Failed to get the mutable reference to the free throw"))
        .update(pins);
    }

    /// A a throw to the last available frame.
    pub fn push_throws_to_last_frame(&mut self, throws: &[Rc<throw::Throw>]) {
        if self.frames.len() == 0 || self.frames.last().unwrap().is_closed() {
            let last_id = self.frames.len();
            let mut new_frame = frame::Frame::new(last_id + 1);
            for throw in throws {
                new_frame.add_throw(&throw.clone());
            }
            self.frames.push(new_frame);
        } else {
            let last_frame = self.frames.last_mut().unwrap();
            for throw in throws {
                last_frame.add_throw(&throw.clone());
            }
        }
    }

    pub fn push_free_throws(&mut self, throws: &[Rc<throw::Throw>]) {
        for throw in throws.iter().filter(|throw| throw.is_free_throw()) {
            self.throws.push(throw.clone());
        }
    }

    /// Return the game score, but only if the game is fininshed.
    pub fn score(&self) -> Option<u16> {
        if !self.is_finished() {
            None
        } else {
            Some(self.frames.iter().map(|x| x.score().unwrap_or(0)).sum())
        }
    }

    /// Check if the game is fininshed.
    pub fn is_finished(&self) -> bool {
        self.frames.len() == 10
            && self
                .frames
                .iter()
                .last()
                .map_or(false, |frame| frame.is_closed())
    }

    pub fn spare(&self, pins: u16) -> bool {
        self.frames
            .last()
            .map_or(false, |frame| frame.possible_spare(pins))
    }

    /// Check if there are any free throws available.
    pub fn has_free_throws(&self) -> bool {
        self.throws.iter().any(|throw| throw.is_free_throw())
    }
}

#[cfg(test)]
mod game_internal_tests {
    use std::rc::Rc;

    #[test]
    pub fn unfinished_game_not_enough_frames() {
        let game = super::BowlingGame::new();
        assert!(!game.is_finished());
    }

    #[test]
    pub fn unfinished_game_free_throws_last_frame() {
        let mut game = super::BowlingGame::new();

        game.frames.push(super::frame::Frame::new(1));
        game.frames.push(super::frame::Frame::new(2));
        game.frames.push(super::frame::Frame::new(3));
        game.frames.push(super::frame::Frame::new(4));
        game.frames.push(super::frame::Frame::new(5));
        game.frames.push(super::frame::Frame::new(6));
        game.frames.push(super::frame::Frame::new(7));
        game.frames.push(super::frame::Frame::new(8));
        game.frames.push(super::frame::Frame::new(9));

        let mut final_frame = super::frame::Frame::new(10);
        final_frame.add_throw(&Rc::new(super::throw::Throw::new(1, 5)));
        final_frame.add_throw(&Rc::new(super::throw::Throw::new_free(2)));
        game.frames.push(final_frame);

        assert!(!game.is_finished());
    }

    #[test]
    pub fn finished_game() {
        let mut game = super::BowlingGame::new();

        game.frames.push(super::frame::Frame::new(1));
        game.frames.push(super::frame::Frame::new(2));
        game.frames.push(super::frame::Frame::new(3));
        game.frames.push(super::frame::Frame::new(4));
        game.frames.push(super::frame::Frame::new(5));
        game.frames.push(super::frame::Frame::new(6));
        game.frames.push(super::frame::Frame::new(7));
        game.frames.push(super::frame::Frame::new(8));
        game.frames.push(super::frame::Frame::new(9));

        let mut final_frame = super::frame::Frame::new(10);
        final_frame.add_throw(&Rc::new(super::throw::Throw::new(1, 5)));
        final_frame.add_throw(&Rc::new(super::throw::Throw::new(2, 3)));
        game.frames.push(final_frame);

        assert!(game.is_finished());
    }
}
