#ifndef SFML_VERTEX_WRAPPER_H
#define SFML_VERTEX_WRAPPER_H

#include <SFML/Graphics/Vertex.h>

void sfVertex_getPosition_wrapper(const sfVertex* vertex, sfVector2f* position);

void sfVertex_setPosition_wrapper(sfVertex* vertex, const sfVector2f* position);

void sfVertex_getColor_wrapper(const sfVertex* vertex, sfColor* color);

void sfVertex_setColor_wrapper(sfVertex* vertex, const sfColor* color);

void sfVertex_getTextureCoords_wrapper(const sfVertex* vertex, sfVector2f* textureCoords);

void sfVertex_setTextureCoords_wrapper(sfVertex* vertex, const sfVector2f* textureCoords);

#endif // SFML_VERTEX_WRAPPER_H
