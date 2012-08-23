#ifndef SFML_COLOR_WRAPPER_H
#define SFML_COLOR_WRAPPER_H

#include <SFML/Graphics/Color.h>

void sfColor_fromRGB_wrapper(sfUint8 red, sfUint8 green, sfUint8 blue, sfColor* color);

void sfColor_fromRGBA_wrapper(sfUint8 red, sfUint8 green, sfUint8 blue, sfUint8 alpha, sfColor* color);

void sfColor_add_wrapper(sfColor* color1, sfColor* color2, sfColor* color);

void sfColor_modulate_wrapper(sfColor* color1, sfColor* color2, sfColor* color);

#endif // SFML_COLOR_WRAPPER_H
