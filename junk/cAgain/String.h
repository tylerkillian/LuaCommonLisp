#ifndef _STRING_H_
#define _STRING_H_

typedef struct {
	char *value;
} String;

#define sp(s) (s->value)

String* String_new();
void String_delete(String *s);

#define sac(a, b) (String_appendCharacter(a, b))
void String_appendCharacter(String *a, char b);

#define sec(a, b) (String_equalsCharacterArray(a, b))
int String_equalsCharacterArray(String *a, char *b);

#endif
