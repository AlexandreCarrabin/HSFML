#ifndef SFML_SHADER_WRAPPER_H
#define SFML_SHADER_WRAPPER_H

#include <SFML/Graphics/Shader.h>

void sfShader_setVector2Parameter_wrapper(sfShader* shader, const char* name, const sfVector2f* vector);

void sfShader_setVector3Parameter_wrapper(sfShader* shader, const char* name, const sfVector3f* vector);

void sfShader_setColorParameter_wrapper(sfShader* shader, const char* name, const sfColor* color);

void sfShader_setTransformParameter_wrapper(sfShader* shader, const char* name, const sfTransform* transform);

#endif // SFML_SHADER_WRAPPER_H
