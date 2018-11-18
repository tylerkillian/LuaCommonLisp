#include <test_all.h>
#include <assert.h>

void test_getToken() {
	char* expression = "(setf a 2)\n(setf b 3)\n(setf c (+ a b))\nc"; 
	char* result = read(expression);
	assert(result == "(setf a 2)");
}

void test_getRemainingExpressionAfterRead() {
	char* expression = "(setf a 2)\n(setf b 3)\n(setf c (+ a b))\nc"; 
	char* result = afterRead(expression);
	assert(result == "(setf b 3)\n(setf c (+ a b))\nc");
}

void test_all() {
	test_getToken();
	test_getRemainingExpressionAfterRead();
}

