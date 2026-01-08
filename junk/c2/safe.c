#include <stdio.h>
#include <safe.h>
#include <assert.h>
#include <boolean.h>
#include <stdlib.h>
#include <string.h>

void **g_blocks = NULL;
int g_num_blocks = 0;

void _add_g_block(void *ptr) {
	void **blocks;

	blocks = (void**)malloc((g_num_blocks + 1) * sizeof(void*));
	memcpy(blocks, g_blocks, g_num_blocks * sizeof(void*));
	blocks[g_num_blocks] = ptr;

	if (g_blocks != NULL) {
		free(g_blocks);
	}

	g_blocks = blocks;
	g_num_blocks++;
}

void _remove_g_block(void *ptr) {
	int index;
	void **blocks;

	index = -1;
	for (index = 0; index < g_num_blocks; index++) {
		if (g_blocks[index] == ptr) {
			break;
		}
	}
	assert(index >= 0);

	if (g_num_blocks > 1) {
		blocks = (void**)malloc((g_num_blocks - 1) * sizeof(void*));
	} else {
		blocks = NULL;
	}

	memcpy(blocks, g_blocks, index * sizeof(void*));
	memcpy(blocks + index, g_blocks + index + 1, (g_num_blocks - index - 1) * sizeof(void*));

	free(g_blocks);

	g_blocks = blocks;
	g_num_blocks--;
}

void safe_assert_empty() {
	assert(g_num_blocks == 0);
	assert(g_blocks == NULL);
}

void* safe_malloc(size_t size) {
	void *result;

	result = malloc(size);
	_add_g_block(result);
	return result;
}

void safe_free(void *ptr) {
	_remove_g_block(ptr);
	free(ptr);
}
