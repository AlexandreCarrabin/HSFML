#include <SFML/Graphics/CircleShapeWrapper.h>

void sfCircleShape_setPosition_wrapper(sfCircleShape* shape, const sfVector2f* position) {
    sfCircleShape_setPosition(shape, *position);
}

void sfCircleShape_setScale_wrapper(sfCircleShape* shape, const sfVector2f* scale) {
    sfCircleShape_setScale(shape, *scale);
}

void sfCircleShape_setOrigin_wrapper(sfCircleShape* shape, const sfVector2f* origin) {
    sfCircleShape_setOrigin(shape, *origin);
}

void sfCircleShape_getPosition_wrapper(const sfCircleShape* shape, sfVector2f* position) {
    *position = sfCircleShape_getPosition(shape);
}

void sfCircleShape_getScale_wrapper(const sfCircleShape* shape, sfVector2f* scale) {
    *scale = sfCircleShape_getScale(shape);
}

void sfCircleShape_getOrigin_wrapper(const sfCircleShape* shape, sfVector2f* origin) {
    *origin = sfCircleShape_getOrigin(shape);
}

void sfCircleShape_move_wrapper(sfCircleShape* shape, const sfVector2f* offset) {
    sfCircleShape_move(shape, *offset);
}

void sfCircleShape_scale_wrapper(sfCircleShape* shape, const sfVector2f* factors) {
    sfCircleShape_scale(shape, *factors);
}

void sfCircleShape_getTransform_wrapper(const sfCircleShape* shape, sfTransform* transform) {
    *transform = sfCircleShape_getTransform(shape);
}

void sfCircleShape_getInverseTransform_wrapper(const sfCircleShape* shape, sfTransform* transform) {
    *transform = sfCircleShape_getInverseTransform(shape);
}

void sfCircleShape_setTextureRect_wrapper(sfCircleShape* shape, const sfIntRect* rectangle) {
    sfCircleShape_setTextureRect(shape, *rectangle);
}

void sfCircleShape_setFillColor_wrapper(sfCircleShape* shape, const sfColor* color) {
    sfCircleShape_setFillColor(shape, *color);
}

void sfCircleShape_setOutlineColor_wrapper(sfCircleShape* shape, const sfColor* color) {
    sfCircleShape_setOutlineColor(shape, *color);
}

void sfCircleShape_getTextureRect_wrapper(const sfCircleShape* shape, sfIntRect* rectangle) {
    *rectangle = sfCircleShape_getTextureRect(shape);
}

void sfCircleShape_getFillColor_wrapper(const sfCircleShape* shape, sfColor* color) {
    *color = sfCircleShape_getFillColor(shape);
}

void sfCircleShape_getOutlineColor_wrapper(const sfCircleShape* shape, sfColor* color) {
    *color = sfCircleShape_getOutlineColor(shape);
}

void sfCircleShape_getPoint_wrapper(const sfCircleShape* shape, unsigned int index, sfVector2f* point) {
    *point = sfCircleShape_getPoint(shape, index);
}

void sfCircleShape_getLocalBounds_wrapper(const sfCircleShape* shape, sfFloatRect* bounds) {
    *bounds = sfCircleShape_getLocalBounds(shape);
}

void sfCircleShape_getGlobalBounds_wrapper(const sfCircleShape* shape, sfFloatRect* bounds) {
    *bounds = sfCircleShape_getGlobalBounds(shape);
}

