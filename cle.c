#include <load.h>
#include <safe.h>

int main(int argc, char **argv) {
	char *filename = argv[1];
	load(filename);

	safe_assert_empty();
	return 0;
}
