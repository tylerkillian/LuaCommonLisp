#include <ut_str.h>
#include <assert.h>
#include <str.h>
#include <string.h>

static void _test_append() {
	str *s;
	char initial[] = "\0",
	     final[] = "abc\0";

	s = str_alloc();

	assert(strcmp(s->v, initial) == 0);
	assert(str_getLength(s) == 0);

	str_append(s, 'a');
	str_append(s, 'b');
	str_append(s, 'c');
	assert(strcmp(s->v, final) == 0);
	assert(str_getLength(s) == 3);

	str_free(s);
}

void ut_str_runTests() {
	_test_append();
}
