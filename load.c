#include <load.h>
#include <stdio.h>
#include <stdlib.h>
#include <safe.h>
#include <str.h>

str* readFile(char *filename) {
	char c;
	str *result;
	FILE *input;

	result = str_alloc();

	input = fopen(filename, "r");
	c = fgetc(input);
	while (c != EOF) {
		str_append(result, c);
		c = fgetc(input);
	}
	fclose(input);

	return result;
}

void load(char *filename) {
	int index;
	char character;
	str *contents;

	contents = readFile(filename);

	for (index = 0; index < str_getLength(contents); index++) {
		character = str_getCharacter(contents, index);
		printf("%c\n", character);
		if (character == '(') {
			printf("  got parenthesis\n");
		}
	}

	str_free(contents);
}
