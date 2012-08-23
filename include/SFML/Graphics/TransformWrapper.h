#ifndef SFML_TRANSFORM_WRAPPER_H
#define SFML_TRANSFORM_WRAPPER_H

#include <SFML/Graphics/Transform.h>

const float* sfTransform_getInternalMatrix(const sfTransform* transform);

void sfTransform_setInternalMatrix(sfTransform* transform, float* matrix);

void sfTransform_fromMatrix_wrapper(sfTransform* transform, 
        float a00, float a01, float a02, 
        float a10, float a11, float a12, 
        float a20, float a21, float a22);

void sfTransform_getInverse_wrapper(const sfTransform* transform, sfTransform* result);

void sfTransform_transformPoint_wrapper(const sfTransform* transform, const sfVector2f* point, sfVector2f* result);

void sfTransform_transformRect_wrapper(const sfTransform* transform, const sfFloatRect* rectangle, sfFloatRect* result);

#endif // SFML_TRANSFORM_WRAPPER_H
