// This stub file contains items which aren't used yet; feel free to remove this module attribute
// to enable stricter warnings.
#![allow(unused)]

pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        if self.health == 0 {
            Some(Self {
                health: 100,
                mana: Some(100),
                level: self.level,
            })
        } else {
            None
        }
    }

    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            Some(x) if x >= mana_cost => {
                let remaining_mana = x - mana_cost;
                if remaining_mana > 0 {
                    self.mana = Some(remaining_mana);
                } else {
                    self.mana = None;
                }
                mana_cost * 2
            }
            Some(x) => 0,
            None => {
                self.health = self.health.saturating_sub(mana_cost);
                0
            }
        }
    }
}
