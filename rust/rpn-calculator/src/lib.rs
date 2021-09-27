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
    let last_result = inputs.iter().fold(None, |_, entry| match entry {
        CalculatorInput::Value(x) => {
            stack.push(*x);
            Some(*x)
        }
        CalculatorInput::Add => {
            let val1 = stack.pop()?;
            let val2 = stack.pop()?;
            let result = val2 + val1;
            stack.push(result);
            Some(result)
        }
        CalculatorInput::Subtract => {
            let val1 = stack.pop()?;
            let val2 = stack.pop()?;
            let result = val2 - val1;
            stack.push(result);
            Some(result)
        }
        CalculatorInput::Multiply => {
            let val1 = stack.pop()?;
            let val2 = stack.pop()?;
            let result = val2 * val1;
            stack.push(result);
            Some(result)
        }
        CalculatorInput::Divide => {
            let val1 = stack.pop()?;
            let val2 = stack.pop()?;
            let result = val2 / val1;
            stack.push(result);
            Some(result)
        }
    });

    if stack.len() == 1 {
        last_result
    } else {
        // too many operands
        None
    }
}
