#include <load.h>
#include <stdio.h>
#include <stdlib.h>
#include <safe.h>

int getFileLength(char *filename) {
	int result = 0;
	FILE *input = fopen(filename, "r");
	char c = fgetc(input);
	while (c != EOF) {
		c = fgetc(input);
		result++;
	}
	fclose(input);
	return result;
}

char* readFile(char *filename) {
	int length = getFileLength(filename);
	char *result = (char*)safe_malloc((length + 1) * sizeof(char));

	FILE *input = fopen(filename, "r");
	int offset = 0;
	for (offset = 0; offset < length; offset++) {
		result[offset] = fgetc(input);
	}
	result[length] = '\0';
	fclose(input);

	return result;
}

void load(char *filename) {
	char *contents = readFile(filename);
	printf("contents = %s", contents);
	safe_free(contents);
}
