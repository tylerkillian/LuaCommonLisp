#include <lispRead.h>
#include <String.h>
#include <Symbol.h>
#include <Object.h>
#include <stdlib.h>
#include <string.h>

void _convertTokenToObject(String *token, Object *result) {
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
		_convertTokenToObject(token, result);
	}
	String_delete(token);
}
