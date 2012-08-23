#ifndef SFML_RENDERTEXTURE_WRAPPER_H
#define SFML_RENDERTEXTURE_WRAPPER_H

#include <SFML/Graphics/RenderTexture.h>

void sfRenderTexture_getSize_wrapper(const sfRenderTexture* renderTexture, sfVector2u* size);

void sfRenderTexture_clear_wrapper(sfRenderTexture* renderTexture, const sfColor* color);

void sfRenderTexture_getViewport_wrapper(const sfRenderTexture* renderTexture, const sfView* view, sfIntRect* viewport);

void sfRenderTexture_convertCoords_wrapper(const sfRenderTexture* renderTexture, const sfVector2i* point, const sfView* targetView, sfVector2f* out);

#endif // SFML_RENDERTEXTURE_WRAPPER_H
