#include <test_all.h>
#include <assert.h>
#include <stdlib.h>

char* read(char* expression) {
	char *result = "(setf a 2)";
	return result;
}

char* afterRead(char* expression) {
	char *result = "(setf b 3)\n(setf c (+ a b))\nc";
	return result;
}

void test_getToken() {
	char *expression = "(setf a 2)\n(setf b 3)\n(setf c (+ a b))\nc"; 
	char *result = read(expression);
	assert(result == "(setf a 2)");
	//free(result);
	free(expression);
}

void test_getRemainingExpressionAfterRead() {
	char *expression = "(setf a 2)\n(setf b 3)\n(setf c (+ a b))\nc"; 
	char *result = afterRead(expression);
	assert(result == "(setf b 3)\n(setf c (+ a b))\nc");
	//free(result);
	//free(expression);
}

void test_all() {
	test_getToken();
	test_getRemainingExpressionAfterRead();
}

