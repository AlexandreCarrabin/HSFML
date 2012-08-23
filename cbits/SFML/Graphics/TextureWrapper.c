#include <SFML/Graphics/TextureWrapper.h>

void sfTexture_getSize_wrapper(const sfTexture* texture, sfVector2u* size) {
    *size = sfTexture_getSize(texture);
}

