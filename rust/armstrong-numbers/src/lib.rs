pub fn is_armstrong_number(num: u32) -> bool {
    let str_num = num.to_string();
    let num_digits = str_num.len() as u32;
    num == str_num
        .chars()
        .map(|x| x.to_digit(10).unwrap())
        .map(|n| n.pow(num_digits))
        .sum()
}
