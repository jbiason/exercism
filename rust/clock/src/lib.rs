#[derive(Eq, PartialEq, Debug)]
pub struct Clock {
    minutes: i32,
}

const MINUTES_IN_AN_HOUR: i32 = 60;
const HOURS_IN_A_DAY: i32 = 24;
const MINUTES_IN_A_DAY: i32 = HOURS_IN_A_DAY * MINUTES_IN_AN_HOUR;

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Self {
            minutes: Self::as_minutes(hours, minutes),
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Self::new(0, self.minutes + minutes)
    }

    fn as_minutes(hours: i32, minutes: i32) -> i32 {
        ((hours * MINUTES_IN_AN_HOUR + minutes) % MINUTES_IN_A_DAY + MINUTES_IN_A_DAY)
            % MINUTES_IN_A_DAY
    }

    fn hours(&self) -> i32 {
        self.minutes / MINUTES_IN_AN_HOUR
    }

    fn minutes(&self) -> i32 {
        self.minutes % MINUTES_IN_AN_HOUR
    }
}

impl std::fmt::Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02}:{:02}", self.hours(), self.minutes())
    }
}
