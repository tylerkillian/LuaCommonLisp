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

_isConstituentCharacter(char c) {
	if ((c >= 'a') && (c <= 'z')) {
		return 1;
	}
	if ((c >= 'A') && (c <= 'Z')) {
		return 1;
	}
	return 0;
}

void lispRead(InputStream *inputStream, Object *result) {
	char nextCharacter;
	String *token = NULL;

	token = String_new();
	while (InputStream_readNextCharacter(inputStream, &nextCharacter) != END_OF_FILE) {
		if (_isConstituentCharacter(nextCharacter) == 1) {
			sac(token, nextCharacter);
		}
		else {
			_convertTokenToObject(token, result);
			break;
		}
	}
	String_delete(token);
}
