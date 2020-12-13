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

int _isConstituentCharacter(char c) {
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
		if (_isWhitespace(nextCharacter) == 1) {
			break;
		}
		else if (_isConstituentCharacter(nextCharacter) == 1) {
			sac(token, nextCharacter);
		}
		else {
			break;
		}
	}
	_convertTokenToObject(token, result);
	String_delete(token);
}
