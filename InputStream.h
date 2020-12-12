#ifndef _INPUTSTREAM_H_
#define _INPUTSTREAM_H_

typedef struct {
	int position;
	int length;
	char values[6];
} InputStream;

InputStream* InputStream_new();
void InputStream_delete(InputStream *inputStream);
int InputStream_readNextCharacter(InputStream *inputStream, char *result);

#endif
