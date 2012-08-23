#include <SFML/Graphics/VertexWrapper.h>

void sfVertex_getPosition_wrapper(const sfVertex* vertex, sfVector2f* position) {
    *position = vertex->position;
}

void sfVertex_setPosition_wrapper(sfVertex* vertex, const sfVector2f* position) {
    vertex->position = *position;
}

void sfVertex_getColor_wrapper(const sfVertex* vertex, sfColor* color) {
    *color = vertex->color;
}

void sfVertex_setColor_wrapper(sfVertex* vertex, const sfColor* color) {
    vertex->color = *color;
}

void sfVertex_getTextureCoords_wrapper(const sfVertex* vertex, sfVector2f* textureCoords) {
    *textureCoords = vertex->texCoords;
}

void sfVertex_setTextureCoords_wrapper(sfVertex* vertex, const sfVector2f* textureCoords) {
    vertex->texCoords = *textureCoords;
}

