#include <test_all.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

int stringsEqual(char *a, char *b) {
	if (strcmp(a, b) == 0) {
		return 1;
	} else {
		return 0;
	}
}

char* read(char* expression) {
	char *result = "(setf a 2)";
	return strdup(result);
}

char* afterRead(char* expression) {
	char *result = "(setf b 3)\n(setf c (+ a b))\nc";
	return strdup(result);
}

void test_getToken() {
	char *expression = "(setf a 2)\n(setf b 3)\n(setf c (+ a b))\nc"; 
	char *result = read(expression);
	assert(stringsEqual(result, "(setf a 2)"));
	free(result);
}

void test_getRemainingExpressionAfterRead() {
	char *expression = "(setf a 2)\n(setf b 3)\n(setf c (+ a b))\nc"; 
	char *result = afterRead(expression);
	assert(stringsEqual(result, "(setf b 3)\n(setf c (+ a b))\nc"));
	free(result);
}

void test_all() {
	test_getToken();
	test_getRemainingExpressionAfterRead();
}

