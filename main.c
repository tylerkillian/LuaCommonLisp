#include <stdio.h>

int main() {
	char character;
	FILE *input;
       
	input = fopen("source.lsp", "r");
	while (fscanf(input, "%c", &character) != EOF) {
		printf("[%c]\n", character);
	}
	fclose(input);
	return 0;
}
