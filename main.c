#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
	NUMBER,
	SYMBOL
} ObjectType;

typedef struct {
	ObjectType itsType;
	void* object;
	void (*delete)(void *object);
} Object;

Object* Object_new(void* object, void (*delete)(void* object)) {
	Object *result = NULL;

	result = (Object*)malloc(sizeof(Object));
	result->object = object;
	result->delete = delete;
	return result;
}

void Object_delete(Object *object) {
	object->delete(object->object);
	free(object);
}

typedef struct {
	int position;
	int length;
	char values[6];
} InputStream;

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

typedef struct {
	char value[255];
} Token;

Token* Token_new() {
	Token *result = NULL;

	result = (Token*)malloc(sizeof(Token));
	result->value[0] = '\0';
	return result;
}

void Token_delete(Token *token) {
	free(token);
}

void Token_addCharacter(Token *token, char character) {
	int characterIndex = 0;

	while (token->value[characterIndex] != '\0') {
		characterIndex++
	}
	token->value[characterIndex] = character;
	token->value[characterIndex + 1] = '\0';
}

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

void convertTokenToObject(Token *token, Object *result) {
	Symbol *symbol = NULL;
	
	symbol = Symbol_new();
	strcpy(symbol->value, token->value);
	result->object = symbol;
	result->delete = Symbol_delete;
}

int read(InputStream *inputStream, Object *result) {
	char nextCharacter;

	Token *token = Token_new();
	while (InputStream_readNextCharacter(inputStream, &nextCharacter) != 0) {
		Token_addCharacter(token, nextCharacter);
		convertTokenToObject(token, result);
	}
	Token_delete(token);
}

int main() {
	InputStream *inputStream;
	Object *object;

	inputStream = InputStream_new();
	object = Object_new();
	read(inputStream, object);
	Object_delete(object);
	InputStream_delete(inputStream);

	return 0;
}