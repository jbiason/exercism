import java.time.LocalDateTime
import java.time.LocalDate

class Gigasecond {

    static long GIGASECOND = 1_000_000_000

    static LocalDateTime add(LocalDate initial) {
        add(initial?.atStartOfDay())
    }

    static LocalDateTime add(LocalDateTime initial) {
        initial?.plusSeconds(GIGASECOND)
    }

}
