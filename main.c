#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ut_String.h>
#include <String.h>
#include <InputStream.h>
#include <ut_InputStream.h>
#include <lispRead.h>
#include <Object.h>
#include <Symbol.h>

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
