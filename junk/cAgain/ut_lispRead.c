#include <ut_lispRead.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ut_String.h>
#include <String.h>
#include <InputStream.h>
#include <ut_InputStream.h>
#include <Object.h>
#include <Symbol.h>
#include <lispRead.h>
#include <assert.h>

void test_readSymbol() {
        InputStream *inputStream;
        Object *object;

        inputStream = InputStream_new();
        object = Object_new();
        lispRead(inputStream, object);
        assert(strcmp(((Symbol*)(object->value))->value, "hello") == 0);
        Object_delete(object);
        InputStream_delete(inputStream);
}

void ut_lispRead_runTests() {
	test_readSymbol();
}
