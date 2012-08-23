#include <SFML/Graphics/TransformableWrapper.h>

void sfTransformable_setPosition_wrapper(sfTransformable* transformable, const sfVector2f* position) {
    sfTransformable_setPosition(transformable, *position);
}

void sfTransformable_setScale_wrapper(sfTransformable* transformable, const sfVector2f* scale) {
    sfTransformable_setScale(transformable, *scale);
}

void sfTransformable_setOrigin_wrapper(sfTransformable* transformable, const sfVector2f* origin) {
    sfTransformable_setOrigin(transformable, *origin);
}

void sfTransformable_getPosition_wrapper(const sfTransformable* transformable, sfVector2f* position) {
    *position = sfTransformable_getPosition(transformable);
}

void sfTransformable_getScale_wrapper(const sfTransformable* transformable, sfVector2f* scale) {
    *scale = sfTransformable_getScale(transformable);
}

void sfTransformable_getOrigin_wrapper(const sfTransformable* transformable, sfVector2f* origin) {
    *origin = sfTransformable_getOrigin(transformable);
}

void sfTransformable_move_wrapper(sfTransformable* transformable, const sfVector2f* offset) {
    sfTransformable_move(transformable, *offset);
}

void sfTransformable_scale_wrapper(sfTransformable* transformable, const sfVector2f* factors) {
    sfTransformable_scale(transformable, *factors);
}

void sfTransformable_getTransform_wrapper(const sfTransformable* transformable, sfTransform* transform) {
    *transform = sfTransformable_getTransform(transformable);
}

void sfTransformable_getInverseTransform_wrapper(const sfTransformable* transformable, sfTransform* transform) {
    *transform = sfTransformable_getInverseTransform(transformable);
}

