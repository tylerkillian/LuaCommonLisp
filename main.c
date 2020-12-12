#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ut_String.h>
#include <String.h>
#include <InputStream.h>
#include <ut_InputStream.h>

typedef struct {
	char value[255];
} Symbol;

Symbol* Symbol_new() {
	Symbol *result = NULL;

	result = (Symbol*)malloc(sizeof(Symbol));
	result->value[0] = '\0';
	return result;
}

void Symbol_delete(Symbol *symbol) {
	free(symbol);
}

typedef enum {
	NUMBER,
	SYMBOL
} ObjectType;

typedef struct {
	ObjectType itsType;
	void* value;
} Object;

Object* Object_new() {
	Object *result = NULL;

	result = (Object*)malloc(sizeof(Object));
	result->itsType = NUMBER;
	result->value = NULL;
	return result;
}

void Object_set(Object *object, ObjectType itsType, void* value) {
	object->itsType = itsType;
	object->value = value;
}

void Object_delete(Object *object) {
	if (object->value != NULL) {
		if (object->itsType == SYMBOL) {
			Symbol_delete(object->value);
		}
	}
	free(object);
}

void convertTokenToObject(String *token, Object *result) {
	Symbol *symbol = NULL;
	
	symbol = Symbol_new();
	strcpy(symbol->value, sp(token));
	Object_set(result, SYMBOL, symbol);
}

int read(InputStream *inputStream, Object *result) {
	char nextCharacter;

	String *token = String_new();
	while (InputStream_readNextCharacter(inputStream, &nextCharacter) != 0) {
		sac(token, nextCharacter);
		convertTokenToObject(token, result);
	}
	String_delete(token);
}

int main() {
	InputStream *inputStream;
	Object *object;

	inputStream = InputStream_new();
	object = Object_new();
	read(inputStream, object);
	printf("%s\n", ((Symbol*)(object->value))->value);
	Object_delete(object);
	InputStream_delete(inputStream);

	ut_String_runTests();
	ut_InputStream_runTests();

	return 0;
}
