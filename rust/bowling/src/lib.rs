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
}

/// A game frame
mod frame {
    #[deny(warnings, missing_docs)]
    use std::cell::RefCell;
    use std::fmt;
    use std::rc::Rc;

    use super::throw::Throw;

    pub struct Frame {
        id: usize,
        throws: Vec<Rc<RefCell<Throw>>>,
    }

    impl Frame {
        pub fn new(id: usize) -> Self {
            Self {
                id: id,
                throws: vec![],
            }
        }

        pub fn add_throw(&mut self, throw: &Rc<RefCell<Throw>>) {
            self.throws.push(Rc::clone(throw));
        }

        pub fn possible_spare(&self, pins: u16) -> bool {
            self.throws.len() == 1 && self.score().unwrap() + pins == 10
        }

        pub fn score(&self) -> Option<u16> {
            if self.throws.len() == 0
                || self
                    .throws
                    .iter()
                    .any(|throw| throw.borrow().is_free_throw())
            {
                None
            } else {
                Some(
                    self.throws
                        .iter()
                        .map(|throw| throw.borrow().score().unwrap())
                        .sum(),
                )
            }
        }

        pub fn is_closed(&self) -> bool {
            !self.has_free_throws() || self.is_a_strike() || self.throws.len() >= 2
        }

        pub fn accept_more_throws(&self) -> bool {
            self.throws.len() != 3
        }

        fn has_free_throws(&self) -> bool {
            self.throws
                .iter()
                .all(|throw| !throw.borrow().is_free_throw())
        }

        fn is_a_strike(&self) -> bool {
            self.throws.len() == 1 && self.throws[0].borrow().is_strike()
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
}

use std::cell::RefCell;
use std::rc::Rc;

pub struct BowlingGame {
    pub frames: Vec<frame::Frame>,
    pub throws: Vec<Rc<RefCell<throw::Throw>>>,
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
            let mut frame_throws: Vec<Rc<RefCell<throw::Throw>>> = Vec::new();
            if !self.has_free_throws() {
                let throw = self.add_new_throw(pins);
                frame_throws.push(throw);
            } else {
                let reused_throw = self.reuse_free_throw(pins);
                frame_throws.push(reused_throw);
            }

            if pins == 10 {
                // On a strike, the player get two "free" throws.
                let last_id = self.throws.len();
                frame_throws.push(Rc::new(RefCell::new(throw::Throw::new_free(last_id + 1))));
                frame_throws.push(Rc::new(RefCell::new(throw::Throw::new_free(last_id + 2))));
            } else if self.spare(pins) {
                let last_id = self.throws.len();
                frame_throws.push(Rc::new(RefCell::new(throw::Throw::new_free(last_id + 1))));
            }

            self.push_throws_to_last_frame(frame_throws.as_slice());
            self.push_free_throws(frame_throws.as_slice());

            dbg!(&self.throws);
            dbg!(&self.frames);

            Ok(())
        }
    }

    /// Add a new throw to the throw list.
    pub fn add_new_throw(&mut self, pins: u16) -> Rc<RefCell<throw::Throw>> {
        let last_id = self.throws.len() + 1;
        let new_throw = Rc::new(RefCell::new(throw::Throw::new(last_id, pins)));
        self.throws.push(Rc::clone(&new_throw));
        Rc::clone(&new_throw)
    }

    /// Reuse on the free throws in the throw list instead of creating a new one.
    pub fn reuse_free_throw(&self, pins: u16) -> Rc<RefCell<throw::Throw>> {
        let throw = self
            .throws
            .iter()
            .filter(|throw| throw.borrow().is_free_throw())
            .take(1)
            .next()
            .expect("There are no free throws for reuse");

        throw.borrow_mut().update(pins);

        throw.clone()
    }

    /// A a throw to the last available frame.
    pub fn push_throws_to_last_frame(&mut self, throws: &[Rc<RefCell<throw::Throw>>]) {
        if self.frames.len() == 0
            || (self.frames.last().unwrap().is_closed() && self.frames.len() < 10)
        {
            let last_id = self.frames.len();
            let mut new_frame = frame::Frame::new(last_id + 1);
            for throw in throws {
                new_frame.add_throw(&Rc::clone(&throw));
            }
            self.frames.push(new_frame);
        } else if self.frames.last().unwrap().accept_more_throws() {
            let last_frame = self.frames.last_mut().unwrap();
            for throw in throws {
                last_frame.add_throw(&Rc::clone(&throw));
            }
        }
    }

    pub fn push_free_throws(&mut self, throws: &[Rc<RefCell<throw::Throw>>]) {
        for throw in throws.iter().filter(|throw| throw.borrow().is_free_throw()) {
            self.throws.push(Rc::clone(throw));
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
        self.throws
            .iter()
            .any(|throw| throw.borrow().is_free_throw())
    }
}
