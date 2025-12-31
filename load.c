#include <load.h>
#include <stdio.h>
#include <stdlib.h>
#include <safe.h>
#include <str.h>

int getFileLength(char *filename) {
	int result;
	char c;
	FILE *input;

	result = 0;

	input = fopen(filename, "r");
	c = fgetc(input);
	while (c != EOF) {
		c = fgetc(input);
		result++;
	}
	fclose(input);

	return result;
}

char* readFile(char *filename) {
	int length, offset;
	char *result;
	FILE *input;

	length = getFileLength(filename);
	result = (char*)safe_malloc((length + 1) * sizeof(char));

	input = fopen(filename, "r");
	for (offset = 0; offset < length; offset++) {
		result[offset] = fgetc(input);
	}
	result[length] = '\0';
	fclose(input);

	return result;
}

void load(char *filename) {
	char *contents;

	contents = readFile(filename);
	printf("contents = %s", contents);
	safe_free(contents);
}
