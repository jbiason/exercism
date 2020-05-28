#[derive(Debug, PartialEq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

/// A throw; holds the information on how many pins where knocked or None if the throw wasn't done
/// yet.
#[derive(Debug)]
struct Throw(Option<u16>);
impl Throw {
    fn new() -> Self {
        Self(None)
    }

    /// True if the throw is a strike
    fn is_strike(&self) -> bool {
        self.0.unwrap_or(0) == 10
    }

    /// This throw happened.
    fn is_some(&self) -> bool {
        self.0.is_some()
    }

    fn score(&self) -> u16 {
        self.0.unwrap_or(0)
    }
}

enum RollType {
    Normal,
    Streak,
    Strike,
}

/// A normal Roll; the player has the chance of throwing two balls in this.
#[derive(Debug)]
struct Normal(Throw, Throw);
impl Normal {
    fn new() -> Self {
        Self(Throw::new(), Throw::new())
    }

    fn is_complete(&self) -> bool {
        self.0.is_strike() || (self.0.is_some() && self.1.is_some())
    }

    fn roll(&mut self, pins: u16) {
        if self.0.is_some() {
            self.1 = Throw(Some(pins))
        } else {
            self.0 = Throw(Some(pins))
        }
    }

    fn score(&self) -> u16 {
        self.0.score() + self.1.score()
    }

    fn post_streak_score(&self) -> u16 {
        self.0.score() * 2 + self.1.score()
    }

    fn roll_type(&self) -> RollType {
        if self.0.score() == 10 {
            RollType::Strike
        } else {
            if self.0.score() + self.1.score() == 10 {
                RollType::Streak
            } else {
                RollType::Normal
            }
        }
    }
}

/// The last Roll is special: It can be three if the player hits at least one strike or make a
/// streak.
#[derive(Debug)]
struct Last(Throw, Throw, Throw);
impl Last {
    fn new() -> Self {
        Self(Throw::new(), Throw::new(), Throw::new())
    }

    fn is_complete(&self) -> bool {
        self.0.is_some() && self.1.is_some()
    }

    fn roll(&mut self, pins: u16) {
        if self.0.is_some() {
            if self.1.is_some() {
                self.2 = Throw(Some(pins))
            } else {
                self.1 = Throw(Some(pins))
            }
        } else {
            self.0 = Throw(Some(pins))
        }
    }

    fn score(&self) -> u16 {
        self.0.score() + self.1.score() + self.2.score()
    }
}

pub struct BowlingGame {
    frames: [Normal; 9],
    last: Last,
    current_frame: usize,
}

impl BowlingGame {
    pub fn new() -> Self {
        Self {
            frames: [
                Normal::new(),
                Normal::new(),
                Normal::new(),
                Normal::new(),
                Normal::new(),
                Normal::new(),
                Normal::new(),
                Normal::new(),
                Normal::new(),
            ],
            last: Last::new(),
            current_frame: 0,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if pins > 10 {
            Err(Error::NotEnoughPinsLeft)
        } else {
            // is this the last throw or a normal throw?
            if dbg!(self.current_frame == 9) {
                self.roll_last(pins)
            } else {
                self.roll_normal(pins)
            }
        }
    }

    fn roll_last(&mut self, pins: u16) -> Result<(), Error> {
        if self.last.is_complete() {
            Err(Error::GameComplete)
        } else {
            self.last.roll(pins);
            dbg!(&self.last);
            Ok(())
        }
    }

    fn roll_normal(&mut self, pins: u16) -> Result<(), Error> {
        if dbg!(self.frames[self.current_frame].is_complete()) {
            self.current_frame += 1;
            self.roll(pins)
        } else {
            Ok(self.frames[self.current_frame].roll(pins))
        }
    }

    pub fn score(&self) -> Option<u16> {
        // the only way to have a score is when all throws when done. And for that, we know that
        // the last throw must have something.
        if self.last.is_complete() {
            // Accumulator: (total_score_so_far, roll_type_in_the_previous_frame)
            let (total, _) =
                self.frames
                    .iter()
                    .fold((0, RollType::Normal), |(total, previous_roll), frame| {
                        let frame_score = match previous_roll {
                            RollType::Strike => frame.score() * 2,
                            RollType::Streak => frame.post_streak_score(),
                            RollType::Normal => frame.score(),
                        };
                        (total + frame_score, frame.roll_type())
                    });
            Some(total + self.last.score())
        } else {
            None
        }
    }
}
