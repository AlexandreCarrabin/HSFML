#ifndef SFML_CONVEXSHAPE_WRAPPER_H
#define SFML_CONVEXSHAPE_WRAPPER_H

#include <SFML/Graphics/ConvexShape.h>

void sfConvexShape_setPosition_wrapper(sfConvexShape* shape, const sfVector2f* position);

void sfConvexShape_setScale_wrapper(sfConvexShape* shape, const sfVector2f* scale);

void sfConvexShape_setOrigin_wrapper(sfConvexShape* shape, const sfVector2f* origin);

void sfConvexShape_getPosition_wrapper(const sfConvexShape* shape, sfVector2f* position);

void sfConvexShape_getScale_wrapper(const sfConvexShape* shape, sfVector2f* scale);

void sfConvexShape_getOrigin_wrapper(const sfConvexShape* shape, sfVector2f* origin);

void sfConvexShape_move_wrapper(sfConvexShape* shape, const sfVector2f* offset);

void sfConvexShape_scale_wrapper(sfConvexShape* shape, const sfVector2f* factors);

void sfConvexShape_getTransform_wrapper(const sfConvexShape* shape, sfTransform* transform);

void sfConvexShape_getInverseTransform_wrapper(const sfConvexShape* shape, sfTransform* transform);

void sfConvexShape_setTextureRect_wrapper(sfConvexShape* shape, const sfIntRect* rectangle);

void sfConvexShape_setFillColor_wrapper(sfConvexShape* shape, const sfColor* color);

void sfConvexShape_setOutlineColor_wrapper(sfConvexShape* shape, const sfColor* color);

void sfConvexShape_getTextureRect_wrapper(const sfConvexShape* shape, sfIntRect* rectangle);

void sfConvexShape_getFillColor_wrapper(const sfConvexShape* shape, sfColor* color);

void sfConvexShape_getOutlineColor_wrapper(const sfConvexShape* shape, sfColor* color);

void sfConvexShape_getPoint_wrapper(const sfConvexShape* shape, unsigned int index, sfVector2f* point);

void sfConvexShape_setPoint_wrapper(sfConvexShape* shape, unsigned int index, const sfVector2f* point);

void sfConvexShape_getLocalBounds_wrapper(const sfConvexShape* shape, sfFloatRect* bounds);

void sfConvexShape_getGlobalBounds_wrapper(const sfConvexShape* shape, sfFloatRect* bounds);

#endif // SFML_CONVEXSHAPE_WRAPPER_H
