#ifndef SFML_CIRCLESHAPE_WRAPPER_H
#define SFML_CIRCLESHAPE_WRAPPER_H

#include <SFML/Graphics/CircleShape.h>

void sfCircleShape_setPosition_wrapper(sfCircleShape* shape, const sfVector2f* position);

void sfCircleShape_setScale_wrapper(sfCircleShape* shape, const sfVector2f* scale);

void sfCircleShape_setOrigin_wrapper(sfCircleShape* shape, const sfVector2f* origin);

void sfCircleShape_getPosition_wrapper(const sfCircleShape* shape, sfVector2f* position);

void sfCircleShape_getScale_wrapper(const sfCircleShape* shape, sfVector2f* scale);

void sfCircleShape_getOrigin_wrapper(const sfCircleShape* shape, sfVector2f* origin);

void sfCircleShape_move_wrapper(sfCircleShape* shape, const sfVector2f* offset);

void sfCircleShape_scale_wrapper(sfCircleShape* shape, const sfVector2f* factors);

void sfCircleShape_getTransform_wrapper(const sfCircleShape* shape, sfTransform* transform);

void sfCircleShape_getInverseTransform_wrapper(const sfCircleShape* shape, sfTransform* transform);

void sfCircleShape_setTextureRect_wrapper(sfCircleShape* shape, const sfIntRect* rectangle);

void sfCircleShape_setFillColor_wrapper(sfCircleShape* shape, const sfColor* color);

void sfCircleShape_setOutlineColor_wrapper(sfCircleShape* shape, const sfColor* color);

void sfCircleShape_getTextureRect_wrapper(const sfCircleShape* shape, sfIntRect* rectangle);

void sfCircleShape_getFillColor_wrapper(const sfCircleShape* shape, sfColor* color);

void sfCircleShape_getOutlineColor_wrapper(const sfCircleShape* shape, sfColor* color);

void sfCircleShape_getPoint_wrapper(const sfCircleShape* shape, unsigned int index, sfVector2f* point);

void sfCircleShape_getLocalBounds_wrapper(const sfCircleShape* shape, sfFloatRect* bounds);

void sfCircleShape_getGlobalBounds_wrapper(const sfCircleShape* shape, sfFloatRect* bounds);

#endif // SFML_CIRCLESHAPE_WRAPPER_H
