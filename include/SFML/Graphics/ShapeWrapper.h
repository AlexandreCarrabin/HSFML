#ifndef SFML_SHAPE_WRAPPER_H
#define SFML_SHAPE_WRAPPER_H

#include <SFML/Graphics/Shape.h>
#include <SFML/Graphics/Shape_stub.h>
#include <HsFFI.h>

sfVector2f hs_getPointCallback_wrapper(unsigned int index, void* data);

sfShape* sfShape_create_wrapper(HsStablePtr data);

void sfShape_setPosition_wrapper(sfShape* shape, const sfVector2f* position);

void sfShape_setScale_wrapper(sfShape* shape, const sfVector2f* scale);

void sfShape_setOrigin_wrapper(sfShape* shape, const sfVector2f* origin);

void sfShape_getPosition_wrapper(const sfShape* shape, sfVector2f* position);

void sfShape_getScale_wrapper(const sfShape* shape, sfVector2f* scale);

void sfShape_getOrigin_wrapper(const sfShape* shape, sfVector2f* origin);

void sfShape_move_wrapper(sfShape* shape, const sfVector2f* offset);

void sfShape_scale_wrapper(sfShape* shape, const sfVector2f* factors);

void sfShape_getTransform_wrapper(const sfShape* shape, sfTransform* transform);

void sfShape_getInverseTransform_wrapper(const sfShape* shape, sfTransform* transform);

void sfShape_setTextureRect_wrapper(sfShape* shape, const sfIntRect* rectangle);

void sfShape_setFillColor_wrapper(sfShape* shape, const sfColor* color);

void sfShape_setOutlineColor_wrapper(sfShape* shape, const sfColor* color);

void sfShape_getTextureRect_wrapper(const sfShape* shape, sfIntRect* rectangle);

void sfShape_getFillColor_wrapper(const sfShape* shape, sfColor* color);

void sfShape_getOutlineColor_wrapper(const sfShape* shape, sfColor* color);

void sfShape_getPoint_wrapper(const sfShape* shape, unsigned int index, sfVector2f* point);

void sfShape_getLocalBounds_wrapper(const sfShape* shape, sfFloatRect* bounds);

void sfShape_getGlobalBounds_wrapper(const sfShape* shape, sfFloatRect* bounds);

#endif // SFML_SHAPE_WRAPPER_H
