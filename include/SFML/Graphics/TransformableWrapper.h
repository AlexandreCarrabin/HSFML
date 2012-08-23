#ifndef SFML_TRANSFORMABLE_WRAPPER_H
#define SFML_TRANSFORMABLE_WRAPPER_H

#include <SFML/Graphics/Transformable.h>

void sfTransformable_setPosition_wrapper(sfTransformable* transformable, const sfVector2f* position);

void sfTransformable_setScale_wrapper(sfTransformable* transformable, const sfVector2f* scale);

void sfTransformable_setOrigin_wrapper(sfTransformable* transformable, const sfVector2f* origin);

void sfTransformable_getPosition_wrapper(const sfTransformable* transformable, sfVector2f* position);

void sfTransformable_getScale_wrapper(const sfTransformable* transformable, sfVector2f* scale);

void sfTransformable_getOrigin_wrapper(const sfTransformable* transformable, sfVector2f* origin);

void sfTransformable_move_wrapper(sfTransformable* transformable, const sfVector2f* offset);

void sfTransformable_scale_wrapper(sfTransformable* transformable, const sfVector2f* factors);

void sfTransformable_getTransform_wrapper(const sfTransformable* transformable, sfTransform* transform);

void sfTransformable_getInverseTransform_wrapper(const sfTransformable* transformable, sfTransform* transform);

#endif // SFML_TRANSFORMABLE_WRAPPER_H
