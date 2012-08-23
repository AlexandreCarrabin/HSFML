#include <SFML/Graphics/ConvexShapeWrapper.h>

void sfConvexShape_setPosition_wrapper(sfConvexShape* shape, const sfVector2f* position) {
    sfConvexShape_setPosition(shape, *position);
}

void sfConvexShape_setScale_wrapper(sfConvexShape* shape, const sfVector2f* scale) {
    sfConvexShape_setScale(shape, *scale);
}

void sfConvexShape_setOrigin_wrapper(sfConvexShape* shape, const sfVector2f* origin) {
    sfConvexShape_setOrigin(shape, *origin);
}

void sfConvexShape_getPosition_wrapper(const sfConvexShape* shape, sfVector2f* position) {
    *position = sfConvexShape_getPosition(shape);
}

void sfConvexShape_getScale_wrapper(const sfConvexShape* shape, sfVector2f* scale) {
    *scale = sfConvexShape_getScale(shape);
}

void sfConvexShape_getOrigin_wrapper(const sfConvexShape* shape, sfVector2f* origin) {
    *origin = sfConvexShape_getOrigin(shape);
}

void sfConvexShape_move_wrapper(sfConvexShape* shape, const sfVector2f* offset) {
    sfConvexShape_move(shape, *offset);
}

void sfConvexShape_scale_wrapper(sfConvexShape* shape, const sfVector2f* factors) {
    sfConvexShape_scale(shape, *factors);
}

void sfConvexShape_getTransform_wrapper(const sfConvexShape* shape, sfTransform* transform) {
    *transform = sfConvexShape_getTransform(shape);
}

void sfConvexShape_getInverseTransform_wrapper(const sfConvexShape* shape, sfTransform* transform) {
    *transform = sfConvexShape_getInverseTransform(shape);
}

void sfConvexShape_setTextureRect_wrapper(sfConvexShape* shape, const sfIntRect* rectangle) {
    sfConvexShape_setTextureRect(shape, *rectangle);
}

void sfConvexShape_setFillColor_wrapper(sfConvexShape* shape, const sfColor* color) {
    sfConvexShape_setFillColor(shape, *color);
}

void sfConvexShape_setOutlineColor_wrapper(sfConvexShape* shape, const sfColor* color) {
    sfConvexShape_setOutlineColor(shape, *color);
}

void sfConvexShape_getTextureRect_wrapper(const sfConvexShape* shape, sfIntRect* rectangle) {
    *rectangle = sfConvexShape_getTextureRect(shape);
}

void sfConvexShape_getFillColor_wrapper(const sfConvexShape* shape, sfColor* color) {
    *color = sfConvexShape_getFillColor(shape);
}

void sfConvexShape_getOutlineColor_wrapper(const sfConvexShape* shape, sfColor* color) {
    *color = sfConvexShape_getOutlineColor(shape);
}

void sfConvexShape_getPoint_wrapper(const sfConvexShape* shape, unsigned int index, sfVector2f* point) {
    *point = sfConvexShape_getPoint(shape, index);
}

void sfConvexShape_setPoint_wrapper(sfConvexShape* shape, unsigned int index, const sfVector2f* point) {
    sfConvexShape_setPoint(shape, index, *point);
}

void sfConvexShape_getLocalBounds_wrapper(const sfConvexShape* shape, sfFloatRect* bounds) {
    *bounds = sfConvexShape_getLocalBounds(shape);
}

void sfConvexShape_getGlobalBounds_wrapper(const sfConvexShape* shape, sfFloatRect* bounds) {
    *bounds = sfConvexShape_getGlobalBounds(shape);
}

