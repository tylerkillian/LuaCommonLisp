#include <test_all.h>
#include <assert.h>

void test_all() {
	char* expression = "(setf a 2)\n(setf b 3)\n(setf c (+ a b))\nc"; 
	char* result = read(expression);
	assert(result == "(setf a 2)");
}
