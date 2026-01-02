#ifndef _STR_H_
#define _STR_H_

typedef struct {
	char *v;
} str;

str* str_alloc();
int str_getLength(str *s);
void str_append(str *s, char c);
char str_getCharacter(str *s, int index);
void str_free(str* s);

#endif
