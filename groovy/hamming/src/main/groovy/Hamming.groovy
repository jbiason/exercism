class Hamming {

    def distance(strand1, strand2) {
        if (strand1.empty && strand2.empty) {
            0
        } else if (strand1.size() != strand2.size()) {
            throw new ArithmeticException()
        } else {
            [strand1.chars, strand2.chars]
                .transpose()
                .collect { record -> record[0] == record[1]? 0: 1 }
                .sum()
        }
    }

}
