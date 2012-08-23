#ifndef SFML_RECTANGLESHAPE_WRAPPER_H
#define SFML_RECTANGLESHAPE_WRAPPER_H

#include <SFML/Graphics/RectangleShape.h>

void sfRectangleShape_setPosition_wrapper(sfRectangleShape* shape, const sfVector2f* position);

void sfRectangleShape_setScale_wrapper(sfRectangleShape* shape, const sfVector2f* scale);

void sfRectangleShape_setOrigin_wrapper(sfRectangleShape* shape, const sfVector2f* origin);

void sfRectangleShape_getPosition_wrapper(const sfRectangleShape* shape, sfVector2f* position);

void sfRectangleShape_getScale_wrapper(const sfRectangleShape* shape, sfVector2f* scale);

void sfRectangleShape_getOrigin_wrapper(const sfRectangleShape* shape, sfVector2f* origin);

void sfRectangleShape_move_wrapper(sfRectangleShape* shape, const sfVector2f* offset);

void sfRectangleShape_scale_wrapper(sfRectangleShape* shape, const sfVector2f* factors);

void sfRectangleShape_getTransform_wrapper(const sfRectangleShape* shape, sfTransform* transform);

void sfRectangleShape_getInverseTransform_wrapper(const sfRectangleShape* shape, sfTransform* transform);

void sfRectangleShape_setTextureRect_wrapper(sfRectangleShape* shape, const sfIntRect* rectangle);

void sfRectangleShape_setFillColor_wrapper(sfRectangleShape* shape, const sfColor* color);

void sfRectangleShape_setOutlineColor_wrapper(sfRectangleShape* shape, const sfColor* color);

void sfRectangleShape_getTextureRect_wrapper(const sfRectangleShape* shape, sfIntRect* rectangle);

void sfRectangleShape_getFillColor_wrapper(const sfRectangleShape* shape, sfColor* color);

void sfRectangleShape_getOutlineColor_wrapper(const sfRectangleShape* shape, sfColor* color);

void sfRectangleShape_getPoint_wrapper(const sfRectangleShape* shape, unsigned int index, sfVector2f* point);

void sfRectangleShape_setSize_wrapper(sfRectangleShape* shape, const sfVector2f* size);

void sfRectangleShape_getSize_wrapper(const sfRectangleShape* shape, sfVector2f* size);

void sfRectangleShape_getLocalBounds_wrapper(const sfRectangleShape* shape, sfFloatRect* bounds);

void sfRectangleShape_getGlobalBounds_wrapper(const sfRectangleShape* shape, sfFloatRect* bounds);

#endif // SFML_RECTANGLESHAPE_WRAPPER_H
