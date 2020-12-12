#include <ut_String.h>
#include <ut_InputStream.h>
#include <ut_lispRead.h>

int main() {
	ut_String_runTests();
	ut_InputStream_runTests();
	ut_lispRead_runTests();

	return 0;
}
