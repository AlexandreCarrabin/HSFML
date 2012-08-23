#include <SFML/Graphics/RectangleShapeWrapper.h>

void sfRectangleShape_setPosition_wrapper(sfRectangleShape* shape, const sfVector2f* position) {
    sfRectangleShape_setPosition(shape, *position);
}

void sfRectangleShape_setScale_wrapper(sfRectangleShape* shape, const sfVector2f* scale) {
    sfRectangleShape_setScale(shape, *scale);
}

void sfRectangleShape_setOrigin_wrapper(sfRectangleShape* shape, const sfVector2f* origin) {
    sfRectangleShape_setOrigin(shape, *origin);
}

void sfRectangleShape_getPosition_wrapper(const sfRectangleShape* shape, sfVector2f* position) {
    *position = sfRectangleShape_getPosition(shape);
}

void sfRectangleShape_getScale_wrapper(const sfRectangleShape* shape, sfVector2f* scale) {
    *scale = sfRectangleShape_getScale(shape);
}

void sfRectangleShape_getOrigin_wrapper(const sfRectangleShape* shape, sfVector2f* origin) {
    *origin = sfRectangleShape_getOrigin(shape);
}

void sfRectangleShape_move_wrapper(sfRectangleShape* shape, const sfVector2f* offset) {
    sfRectangleShape_move(shape, *offset);
}

void sfRectangleShape_scale_wrapper(sfRectangleShape* shape, const sfVector2f* factors) {
    sfRectangleShape_scale(shape, *factors);
}

void sfRectangleShape_getTransform_wrapper(const sfRectangleShape* shape, sfTransform* transform) {
    *transform = sfRectangleShape_getTransform(shape);
}

void sfRectangleShape_getInverseTransform_wrapper(const sfRectangleShape* shape, sfTransform* transform) {
    *transform = sfRectangleShape_getInverseTransform(shape);
}

void sfRectangleShape_setTextureRect_wrapper(sfRectangleShape* shape, const sfIntRect* rectangle) {
    sfRectangleShape_setTextureRect(shape, *rectangle);
}

void sfRectangleShape_setFillColor_wrapper(sfRectangleShape* shape, const sfColor* color) {
    sfRectangleShape_setFillColor(shape, *color);
}

void sfRectangleShape_setOutlineColor_wrapper(sfRectangleShape* shape, const sfColor* color) {
    sfRectangleShape_setOutlineColor(shape, *color);
}

void sfRectangleShape_getTextureRect_wrapper(const sfRectangleShape* shape, sfIntRect* rectangle) {
    *rectangle = sfRectangleShape_getTextureRect(shape);
}

void sfRectangleShape_getFillColor_wrapper(const sfRectangleShape* shape, sfColor* color) {
    *color = sfRectangleShape_getFillColor(shape);
}

void sfRectangleShape_getOutlineColor_wrapper(const sfRectangleShape* shape, sfColor* color) {
    *color = sfRectangleShape_getOutlineColor(shape);
}

void sfRectangleShape_getPoint_wrapper(const sfRectangleShape* shape, unsigned int index, sfVector2f* point) {
    *point = sfRectangleShape_getPoint(shape, index);
}

void sfRectangleShape_setSize_wrapper(sfRectangleShape* shape, const sfVector2f* size) {
    sfRectangleShape_setSize(shape, *size);
}

void sfRectangleShape_getSize_wrapper(const sfRectangleShape* shape, sfVector2f* size) {
    *size = sfRectangleShape_getSize(shape);
}

void sfRectangleShape_getLocalBounds_wrapper(const sfRectangleShape* shape, sfFloatRect* bounds) {
    *bounds = sfRectangleShape_getLocalBounds(shape);
}

void sfRectangleShape_getGlobalBounds_wrapper(const sfRectangleShape* shape, sfFloatRect* bounds) {
    *bounds = sfRectangleShape_getGlobalBounds(shape);
}

