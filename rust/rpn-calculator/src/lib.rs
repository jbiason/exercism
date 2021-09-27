#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut stack: Vec<i32> = Vec::new();
    let last_result = inputs.iter().fold(None, |current: Option<i32>, entry| {
        match (entry, stack.pop()) {
            (CalculatorInput::Value(x), None) => {
                stack.push(current.unwrap());
                if let Some(y) = current {
                    stack.push(y);
                }
                Some(*x)
            }
            (CalculatorInput::Value(x), Some(y)) => {
                stack.push(y);
                stack.push(current.unwrap());
                Some(*x)
            }
            (CalculatorInput::Add, Some(x)) => Some(current.unwrap() + x),
            (CalculatorInput::Subtract, Some(x)) => Some(x - current.unwrap()),
            (CalculatorInput::Multiply, Some(x)) => Some(x * current.unwrap()),
            (CalculatorInput::Divide, Some(x)) => Some(x / current.unwrap()),
            _ => None,
        }
    });

    if stack.is_empty() {
        last_result
    } else {
        // too many operands
        None
    }
}
