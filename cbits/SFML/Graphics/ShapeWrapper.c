#include <SFML/Graphics/ShapeWrapper.h>

sfVector2f hs_getPointCallback_wrapper(unsigned int index, void* data) {
    sfVector2f position;
    hs_getPointCallback(data, index, &position);
    return position;
}

sfShape* sfShape_create_wrapper(HsStablePtr data) {
    return sfShape_create(&hs_getPointCountCallback, &hs_getPointCallback_wrapper, data);
}

void sfShape_setPosition_wrapper(sfShape* shape, const sfVector2f* position) {
    sfShape_setPosition(shape, *position);
}

void sfShape_setScale_wrapper(sfShape* shape, const sfVector2f* scale) {
    sfShape_setScale(shape, *scale);
}

void sfShape_setOrigin_wrapper(sfShape* shape, const sfVector2f* origin) {
    sfShape_setOrigin(shape, *origin);
}

void sfShape_getPosition_wrapper(const sfShape* shape, sfVector2f* position) {
    *position = sfShape_getPosition(shape);
}

void sfShape_getScale_wrapper(const sfShape* shape, sfVector2f* scale) {
    *scale = sfShape_getScale(shape);
}

void sfShape_getOrigin_wrapper(const sfShape* shape, sfVector2f* origin) {
    *origin = sfShape_getOrigin(shape);
}

void sfShape_move_wrapper(sfShape* shape, const sfVector2f* offset) {
    sfShape_move(shape, *offset);
}

void sfShape_scale_wrapper(sfShape* shape, const sfVector2f* factors) {
    sfShape_scale(shape, *factors);
}

void sfShape_getTransform_wrapper(const sfShape* shape, sfTransform* transform) {
    *transform = sfShape_getTransform(shape);
}

void sfShape_getInverseTransform_wrapper(const sfShape* shape, sfTransform* transform) {
    *transform = sfShape_getInverseTransform(shape);
}

void sfShape_setTextureRect_wrapper(sfShape* shape, const sfIntRect* rectangle) {
    sfShape_setTextureRect(shape, *rectangle);
}

void sfShape_setFillColor_wrapper(sfShape* shape, const sfColor* color) {
    sfShape_setFillColor(shape, *color);
}

void sfShape_setOutlineColor_wrapper(sfShape* shape, const sfColor* color) {
    sfShape_setOutlineColor(shape, *color);
}

void sfShape_getTextureRect_wrapper(const sfShape* shape, sfIntRect* rectangle) {
    *rectangle = sfShape_getTextureRect(shape);
}

void sfShape_getFillColor_wrapper(const sfShape* shape, sfColor* color) {
    *color = sfShape_getFillColor(shape);
}

void sfShape_getOutlineColor_wrapper(const sfShape* shape, sfColor* color) {
    *color = sfShape_getOutlineColor(shape);
}

void sfShape_getPoint_wrapper(const sfShape* shape, unsigned int index, sfVector2f* point) {
    *point = sfShape_getPoint(shape, index);
}

void sfShape_getLocalBounds_wrapper(const sfShape* shape, sfFloatRect* bounds) {
    *bounds = sfShape_getLocalBounds(shape);
}

void sfShape_getGlobalBounds_wrapper(const sfShape* shape, sfFloatRect* bounds) {
    *bounds = sfShape_getGlobalBounds(shape);
}

