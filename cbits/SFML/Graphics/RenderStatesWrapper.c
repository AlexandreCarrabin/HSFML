#include <SFML/Graphics/RenderStatesWrapper.h>

void sfRenderStates_getTransform_wrapper(const sfRenderStates* states, sfTransform* transform) {
    *transform = states->transform;
}

void sfRenderStates_setTransform_wrapper(sfRenderStates* states, const sfTransform* transform) {
    states->transform = *transform;
}

