#include <assert.h>
#include <String.h>

void test_appendCharacter() {
	String *s;
	s = String_new();
	sac(s, 'x');
	sac(s, 'y');
	sac(s, 'z');
	assert(sec(s, "xyz"));
	String_delete(s);
}

void ut_String_run_tests() {
	test_appendCharacter();
}
