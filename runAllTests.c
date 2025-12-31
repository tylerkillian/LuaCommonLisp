#include <assert.h>
#include <load.h>
#include <safe.h>
#include <stdio.h>
#include <stdlib.h>
#include <ut_str.h>

int main(int argc, char **argv) {
	char filename[] = "run-all-tests.lsp";

	assert(argc == 1);
	assert(argv[0] != NULL);

	ut_str_runTests();

        load(filename);

        safe_assert_empty();
	return 0;
}
