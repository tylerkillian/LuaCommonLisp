#ifndef _LISPREAD_H_
#define _LISPREAD_H_
#include <Object.h>
#include <InputStream.h>

int lispRead(InputStream *inputStream, Object *result);

#endif
