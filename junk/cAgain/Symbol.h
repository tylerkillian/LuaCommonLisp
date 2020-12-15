#ifndef _SYMBOL_H_
#define _SYMBOL_H_

typedef struct {
        char value[255];
} Symbol;

Symbol* Symbol_new();
void Symbol_delete(Symbol *symbol);

#endif
