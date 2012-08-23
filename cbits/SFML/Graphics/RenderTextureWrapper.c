#include <SFML/Graphics/RenderTextureWrapper.h>

void sfRenderTexture_getSize_wrapper(const sfRenderTexture* renderTexture, sfVector2u* size) {
    *size = sfRenderTexture_getSize(renderTexture);
}

void sfRenderTexture_clear_wrapper(sfRenderTexture* renderTexture, const sfColor* color) {
    sfRenderTexture_clear(renderTexture, *color);
}

void sfRenderTexture_getViewport_wrapper(const sfRenderTexture* renderTexture, const sfView* view, sfIntRect* viewport) {
    *viewport = sfRenderTexture_getViewport(renderTexture, view);
}

void sfRenderTexture_convertCoords_wrapper(const sfRenderTexture* renderTexture, const sfVector2i* point, const sfView* targetView, sfVector2f* out) {
    *out = sfRenderTexture_convertCoords(renderTexture, *point, targetView);
}

