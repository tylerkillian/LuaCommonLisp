#ifndef _INPUTSTREAM_H_
#define _INPUTSTREAM_H_

typedef enum {
	SUCCESS,
	END_OF_FILE
} ErrorType;

typedef struct {
	int position;
	int length;
	char values[6];
} InputStream;

InputStream* InputStream_new();
void InputStream_delete(InputStream *inputStream);
ErrorType InputStream_readNextCharacter(InputStream *inputStream, char *result);

#endif
