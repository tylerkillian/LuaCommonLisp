#include <stdio.h>
#include <safe.h>
#include <assert.h>
#include <boolean.h>
#include <stdlib.h>

void **g_blocks = NULL;
int g_num_blocks = 0;

void _add_g_block(void *ptr) {
	int index = 0;
	void **old_blocks = g_blocks;

	g_blocks = (void**)malloc((g_num_blocks + 1) * sizeof(void*));
	for (index = 0; index < g_num_blocks; index++) {
		g_blocks[index] = old_blocks[index];
	}
	g_blocks[g_num_blocks] = ptr;

	g_num_blocks++;

	if (old_blocks != NULL) {
		free(old_blocks);
	}
}

void _remove_g_block(void *ptr) {
	int old_index = 0, new_index = 0;
	b found = FALSE;
	void **old_blocks = g_blocks;

	for (old_index = 0; old_index < g_num_blocks; old_index++) {
		if (old_blocks[old_index] == ptr) {
			found = TRUE;
			break;
		}
	}
	assert(found);

	if (g_num_blocks > 1) {
		g_blocks = (void**)malloc((g_num_blocks - 1) * sizeof(void*));
	} else {
		g_blocks = NULL;
	}

	for (old_index = 0; old_index < g_num_blocks; old_index++) {
		if (old_blocks[old_index] != ptr) {
			g_blocks[new_index] = old_blocks[old_index];
			new_index++;
		}
	}

	g_num_blocks--;

	free(old_blocks);
}

void safe_assert_empty() {
	assert(g_num_blocks == 0);
	assert(g_blocks == NULL);
}

void* safe_malloc(size_t size) {
	void *result = malloc(size);
	_add_g_block(result);
	return result;
}

void safe_free(void *ptr) {
	_remove_g_block(ptr);
	free(ptr);
}
