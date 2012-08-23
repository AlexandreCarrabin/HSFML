#include <SFML/Graphics/ColorWrapper.h>

void sfColor_fromRGB_wrapper(sfUint8 red, sfUint8 green, sfUint8 blue, sfColor* color) {
    *color = sfColor_fromRGB(red, green, blue);
}

void sfColor_fromRGBA_wrapper(sfUint8 red, sfUint8 green, sfUint8 blue, sfUint8 alpha, sfColor* color) {
    *color = sfColor_fromRGBA(red, green, blue, alpha);
}

void sfColor_add_wrapper(sfColor* color1, sfColor* color2, sfColor* color) {
    *color = sfColor_add(*color1, *color2);
}
void sfColor_modulate_wrapper(sfColor* color1, sfColor* color2, sfColor* color) {
    *color = sfColor_modulate(*color1, *color2);
}

