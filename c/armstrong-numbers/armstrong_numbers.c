#include "armstrong_numbers.h"
#include <stdio.h>
#include <string.h>
#include <math.h>

bool is_armstrong_number(int candidate) {
	char buffer[15];
	sprintf(buffer, "%d", candidate);
	int exponent = strlen(buffer);
	int sum = 0;
	for (int i = 0; i < exponent; i++) {
		sum += pow(buffer[i] - 48, exponent);
	}

	return candidate == sum;
}
