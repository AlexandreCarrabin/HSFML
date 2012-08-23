#include <SFML/Graphics/ShaderWrapper.h>

void sfShader_setVector2Parameter_wrapper(sfShader* shader, const char* name, const sfVector2f* vector) {
    sfShader_setVector2Parameter(shader, name, *vector);
}

void sfShader_setVector3Parameter_wrapper(sfShader* shader, const char* name, const sfVector3f* vector) {
    sfShader_setVector3Parameter(shader, name, *vector);
}

void sfShader_setColorParameter_wrapper(sfShader* shader, const char* name, const sfColor* color) {
    sfShader_setColorParameter(shader, name, *color);
}

void sfShader_setTransformParameter_wrapper(sfShader* shader, const char* name, const sfTransform* transform) {
    sfShader_setTransformParameter(shader, name, *transform);
}

