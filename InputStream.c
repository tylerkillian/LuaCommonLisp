#include <InputStream.h>
#include <stdlib.h>
#include <string.h>
#include <String.h>

InputStream* InputStream_new() {
	InputStream *result = NULL;

	result = (InputStream*)malloc(sizeof(InputStream));
	result->position = 0;
	result->length = 5;
	strcpy(result->values, "hello");
	return result;
}

void InputStream_delete(InputStream *inputStream) {
	free(inputStream);
}

int InputStream_readNextCharacter(InputStream *inputStream, char *result) {
	if (inputStream->position >= inputStream->length) {
		return 0;
	}

	*result = inputStream->values[inputStream->position];
	inputStream->position++;
	return 1;
}
