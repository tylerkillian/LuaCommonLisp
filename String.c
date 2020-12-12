#include <String.h>

String* String_new() {
	String *result;

	result = (String*)malloc(sizeof(String));
	result->value = NULL;
	return result;
}

void String_delete(String *s) {
	if (s->value != NULL) {
		free(s->value);
	}
	free(s);
}

void String_appendCharacter(String *a, char b) {
	int newLength = 0;
	char *new = NULL;
	char *old = NULL;

	newLength = a->value == NULL ? 1 : strlen(a->value) + 1;
	new = (char*)malloc((newLength + 1)*sizeof(char));
	if (a->value != NULL) {
		strcpy(new, a->value);
		free(a->value);
		a->value = NULL;
	}
	a->value = new;
	a->value[newLength - 1] = b;
	a->value[newLength] = '\0';
}

int String_equalsCharacterArray(String *a, char *b) {
	if (strcmp(a->value, b) == 0) {
		return 1;
	}
	return 0;
}
