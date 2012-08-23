#include <SFML/Graphics/ImageWrapper.h>

sfImage* sfImage_createFromColor_wrapper(unsigned int width, unsigned int height, sfColor* color) {
    return sfImage_createFromColor(width, height, *color);
}

void sfImage_createMaskFromColor_wrapper(sfImage* image, sfColor* color, sfUint8 alpha) {
    sfImage_createMaskFromColor(image, *color, alpha);
}

void sfImage_copyImage_wrapper(sfImage* image, const sfImage* source, unsigned int destX, unsigned int destY, sfIntRect* sourceRect, sfBool applyAlpha) {
    sfImage_copyImage(image, source, destX, destY, *sourceRect, applyAlpha);
}

void sfImage_getSize_wrapper(const sfImage* image, sfVector2u* size) {
    *size = sfImage_getSize(image);
}

void sfImage_setPixel_wrapper(sfImage* image, unsigned int x, unsigned int y, sfColor* color) {
    sfImage_setPixel(image, x, y, *color);
}

void sfImage_getPixel_wrapper(sfImage* image, unsigned int x, unsigned int y, sfColor* color) {
    *color = sfImage_getPixel(image, x, y);
}

