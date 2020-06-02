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

        pub fn score(&self) -> Option<u16> {
            if self.throws.len() == 0 || self.throws.iter().any(|throw| throw.is_free_throw()) {
                None
            } else {
                Some(self.throws.iter().map(|throw| throw.score().unwrap()).sum())
            }
        }

        pub fn is_spare(&self) -> bool {
            self.throws.len() == 2 && self.score() == Some(10)
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
    frames: Vec<frame::Frame>,
    throws: Vec<Rc<throw::Throw>>,
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
        } else {
            let throw = self.make_throw(pins);
            let last_id = self.throws.len();
            let frame = self.last_frame();
            frame.add_throw(&throw);

            // check if the player gained any free throws.
            if pins == 10 {
                // a strike, you get 2 free throws
                let new_throw = Rc::new(throw::Throw::new_free(last_id + 1));
                self.throws.push(new_throw.clone());
                frame.add_throw(&new_throw);

                let new_throw = Rc::new(throw::Throw::new_free(last_id + 2));
                self.throws.push(new_throw.clone());
                frame.add_throw(&new_throw);
            } else if frame.is_spare() {
                // in a spare, just one throw
                let new_throw = Rc::new(throw::Throw::new_free(last_id + 1));
                self.throws.push(new_throw.clone());
                frame.add_throw(&new_throw);
            }

            dbg!(&self.throws);
            dbg!(&self.frames);
            Ok(())
        }
    }

    pub fn score(&self) -> Option<u16> {
        None
        // if !self.is_finished() {
        //     None
        // } else {
        //     Some(self.frames.iter().map(|x| x.score()).sum())
        // }
    }

    /// If there are free throws, update the most recent one; if there are none, create a new
    /// throw.
    fn make_throw(&mut self, pins: u16) -> Rc<throw::Throw> {
        let new_id = self.throws.len() + 1;
        self.throws
            .iter_mut()
            .filter(|x| x.is_free_throw())
            .take(1)
            .next()
            .map_or(Rc::new(throw::Throw::new_free(new_id)), |x| {
                let throw = Rc::get_mut(x).unwrap();
                throw.update(pins);
                x.clone()
            })
    }

    /// Check if the list of throws in the game there are at least one free.
    fn has_free_throws(&self) -> bool {
        self.throws.iter().any(|x| x.is_free_throw())
    }

    /// The game is over when there are 10 frames of scores.
    fn is_finished(&self) -> bool {
        self.frames.len() == 10
    }

    /// Get the last/current frame.
    fn last_frame(&mut self) -> &mut frame::Frame {
        if self.frames.len() == 0 {
            let new_id = self.frames.len() + 1;
            self.frames.push(frame::Frame::new(new_id));
        }
        self.frames.iter_mut().last().unwrap()
    }
}
