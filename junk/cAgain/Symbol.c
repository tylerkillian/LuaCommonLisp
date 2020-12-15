#include <Symbol.h>
#include <stdlib.h>
#include <String.h>

Symbol* Symbol_new() {
        Symbol *result = NULL;

        result = (Symbol*)malloc(sizeof(Symbol));
        result->value[0] = '\0';
        return result;
}

void Symbol_delete(Symbol *symbol) {
        free(symbol);
}
