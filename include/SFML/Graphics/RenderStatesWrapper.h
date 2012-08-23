#ifndef SFML_RENDERSTATES_WRAPPER_H
#define SFML_RENDERSTATES_WRAPPER_H

#include <SFML/Graphics/RenderStates.h>

void sfRenderStates_getTransform_wrapper(const sfRenderStates* states, sfTransform* transform);

void sfRenderStates_setTransform_wrapper(sfRenderStates* states, const sfTransform* transform);

#endif // SFML_RENDERSTATES_WRAPPER_H
