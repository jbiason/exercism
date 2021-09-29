#ifndef RESISTOR_COLOR_H
#define RESISTOR_COLOR_H

typedef enum {
	BLACK = 0,
	BROWN,
	RED,
	ORANGE,
	YELLOW,
	GREEN,
	BLUE,
	VIOLET,
	GREY,
	WHITE
} resistor_band_t;

#define color_code(color) color

const resistor_band_t* colors();

#endif
