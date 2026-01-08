#ifndef _SAFE_H_
#define _SAFE_H_

#include <stdlib.h>

void safe_assert_empty();

void* safe_malloc(size_t size);
void safe_free(void *ptr);

#endif
