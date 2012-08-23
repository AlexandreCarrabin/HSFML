#include <SFML/Graphics/VertexArrayWrapper.h>

void sfVertexArray_append_wrapper(sfVertexArray* vertexArray, const sfVertex* vertex) {
    sfVertexArray_append(vertexArray, *vertex);
}

void sfVertexArray_getBounds_wrapper(sfVertexArray* vertexArray, sfFloatRect* bounds) {
    *bounds = sfVertexArray_getBounds(vertexArray);
}

