#include <assert.h>
#include <String.h>

void test_appendCharacter() {
	String *s;
	s = String_new();
	sa(s, 'x');
	sa(s, 'y');
	sa(s, 'z');
	assert(se(s, "xyz"));
}

void ut_String_run_tests() {
	test_appendCharacter();
}
