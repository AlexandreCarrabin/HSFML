#ifndef SFML_IMAGE_WRAPPER_H
#define SFML_IMAGE_WRAPPER_H

#include <SFML/Graphics/Image.h>

sfImage* sfImage_createFromColor_wrapper(unsigned int width, unsigned int height, sfColor* color);

void sfImage_createMaskFromColor_wrapper(sfImage* image, sfColor* color, sfUint8 alpha);

void sfImage_copyImage_wrapper(sfImage* image, const sfImage* source, unsigned int destX, unsigned int destY, sfIntRect* sourceRect, sfBool applyAlpha);

void sfImage_getSize_wrapper(const sfImage* image, sfVector2u* size);

void sfImage_setPixel_wrapper(sfImage* image, unsigned int x, unsigned int y, sfColor* color);

void sfImage_getPixel_wrapper(sfImage* image, unsigned int x, unsigned int y, sfColor* color);

#endif // SFML_IMAGE_WRAPPER_H
