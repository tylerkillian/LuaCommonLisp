#include <assert.h>
#include <cl.h>
#include <safe.h>
#include <stdio.h>
#include <stdlib.h>
#include <ut_str.h>

int main(int argc, char **argv) {
	char *filename;

	assert(argc == 2);
	filename = argv[1];
        cl_main(filename);

	ut_str_runTests();

        safe_assert_empty();
	return 0;
}
