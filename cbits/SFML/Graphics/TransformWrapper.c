#include <SFML/Graphics/TransformWrapper.h>

#include <stdio.h>

const float* sfTransform_getInternalMatrix(const sfTransform* transform) {
    return transform->matrix;
}

void sfTransform_setInternalMatrix(sfTransform* transform, float* matrix) {
    int i;
    for (i = 0; i < 9; i++) {
        transform->matrix[i] = matrix[i];
    }
}

void sfTransform_fromMatrix_wrapper(sfTransform* transform,
        float a00, float a01, float a02, 
        float a10, float a11, float a12, 
        float a20, float a21, float a22) {
    *transform = sfTransform_fromMatrix(a00, a01, a02, a10, a11, a12, a20, a21, a22);
}

void sfTransform_getInverse_wrapper(const sfTransform* transform, sfTransform* result) {
    *result = sfTransform_getInverse(transform);
}

void sfTransform_transformPoint_wrapper(const sfTransform* transform, const sfVector2f* point, sfVector2f* result) {
    *result = sfTransform_transformPoint(transform, *point);
}

void sfTransform_transformRect_wrapper(const sfTransform* transform, const sfFloatRect* rectangle, sfFloatRect* result) {
    *result = sfTransform_transformRect(transform, *rectangle);
}

