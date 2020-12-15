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

int _isWhitespace(char c) {
	if (c == ' ') {
		return 1;
	}
	return 0;
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

int lispRead(InputStream *inputStream, Object *result) {
	char nextCharacter;
	String *token = NULL;

	while (InputStream_readNextCharacter(inputStream, &nextCharacter) != END_OF_FILE) {
		if (_isWhitespace(nextCharacter) == 1) {
			if (token == NULL) {
				continue;
			}
			break;
		}
		else if (_isConstituentCharacter(nextCharacter) == 1) {
			if (token == NULL) {
				token = String_new();
			}
			sac(token, nextCharacter);
		}
		else {
			break;
		}
	}
	if (token != NULL) {
		_convertTokenToObject(token, result);
		String_delete(token);
		return 1;
	}
	return 0;
}
