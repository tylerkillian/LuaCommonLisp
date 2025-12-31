#include <str.h>
#include <safe.h>
#include <stdlib.h>
#include <string.h>

#include <stdio.h>

str* str_alloc() {
	str *result;

	result = (str*)safe_malloc(sizeof(str));
	result->v = (char*)safe_malloc(sizeof(char));
	result->v[0] = '\0';
	return result;
}

int str_getLength(str *s) {
	return strlen(s->v);
}

void str_append(str *s, char c) {
	int length;
	char *v;

	length = strlen(s->v) + 1;
	v = (char*)safe_malloc((length + 1) * sizeof(char));

	strcpy(v, s->v);
	v[length - 1] = c;
	v[length] = '\0';

	safe_free(s->v);

	s->v = v;
}

void str_free(str *s) {
	safe_free(s->v);
	safe_free(s);
}
