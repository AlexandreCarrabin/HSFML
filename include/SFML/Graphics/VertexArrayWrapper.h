#ifndef SFML_VERTEXARRAY_WRAPPER_H
#define SFML_VERTEXARRAY_WRAPPER_H

#include <SFML/Graphics/VertexArray.h>

void sfVertexArray_append_wrapper(sfVertexArray* vertexArray, const sfVertex* vertex);

void sfVertexArray_getBounds_wrapper(sfVertexArray* vertexArray, sfFloatRect* bounds);

#endif // SFML_VERTEXARRAY_WRAPPER_H
