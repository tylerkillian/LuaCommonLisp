#include <Object.h>
#include <stdlib.h>
#include <Symbol.h>

Object* Object_new() {
        Object *result = NULL;

        result = (Object*)malloc(sizeof(Object));
        result->itsType = NUMBER;
        result->value = NULL;
        return result;
}

void Object_set(Object *object, ObjectType itsType, void* value) {
        object->itsType = itsType;
        object->value = value;
}

void Object_delete(Object *object) {
        if (object->value != NULL) {
                if (object->itsType == SYMBOL) {
                        Symbol_delete(object->value);
                }
        }
        free(object);
}
