#include "resistor_color.h"

const resistor_band_t AS_ARRAY[] = {BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE};

const resistor_band_t* colors() {
	return AS_ARRAY;
}
