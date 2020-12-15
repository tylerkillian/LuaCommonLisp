#ifndef _OBJECT_H_
#define _OBJECT_H_

typedef enum {
        NUMBER,
        SYMBOL
} ObjectType;

typedef struct {
        ObjectType itsType;
        void* value;
} Object;

Object* Object_new();
void Object_set(Object *object, ObjectType itsType, void* value);
void Object_delete(Object *object);

#endif
