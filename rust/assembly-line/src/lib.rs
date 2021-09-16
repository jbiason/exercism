// This stub file contains items which aren't used yet; feel free to remove this module attribute
// to enable stricter warnings.
#![allow(unused)]

pub fn production_rate_per_hour(speed: u8) -> f64 {
    speed as f64
        * 221.0
        * match speed {
            x if x >= 5 && x < 9 => 0.9,
            x if x >= 9 => 0.77,
            _ => 1.0,
        }
}

pub fn working_items_per_minute(speed: u8) -> u32 {
    production_rate_per_hour(speed) as u32 / 60
}
