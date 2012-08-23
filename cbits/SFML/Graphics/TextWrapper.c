#include <SFML/Graphics/TextWrapper.h>

void sfText_setPosition_wrapper(sfText* text, const sfVector2f* position) {
    sfText_setPosition(text, *position);
}

void sfText_setScale_wrapper(sfText* text, const sfVector2f* scale) {
    sfText_setScale(text, *scale);
}

void sfText_setOrigin_wrapper(sfText* text, const sfVector2f* origin) {
    sfText_setOrigin(text, *origin);
}

void sfText_getPosition_wrapper(const sfText* text, sfVector2f* position) {
    *position = sfText_getPosition(text);
}

void sfText_getScale_wrapper(const sfText* text, sfVector2f* scale) {
    *scale = sfText_getScale(text);
}

void sfText_getOrigin_wrapper(const sfText* text, sfVector2f* origin) {
    *origin = sfText_getOrigin(text);
}

void sfText_move_wrapper(sfText* text, const sfVector2f* offset) {
    sfText_move(text, *offset);
}

void sfText_scale_wrapper(sfText* text, const sfVector2f* factors) {
    sfText_scale(text, *factors);
}

void sfText_getTransform_wrapper(const sfText* text, sfTransform* transform) {
    *transform = sfText_getTransform(text);
}

void sfText_getInverseTransform_wrapper(const sfText* text, sfTransform* transform) {
    *transform = sfText_getInverseTransform(text);
}

void sfText_setColor_wrapper(sfText* text, const sfColor* color) {
    sfText_setColor(text, *color);
}

void sfText_getColor_wrapper(const sfText* text, sfColor* color) {
    *color = sfText_getColor(text);
}

void sfText_findCharacterPos_wrapper(const sfText* text, size_t index, sfVector2f* position) {
    *position = sfText_findCharacterPos(text, index);
}

void sfText_getLocalBounds_wrapper(const sfText* text, sfFloatRect* bounds) {
    *bounds = sfText_getLocalBounds(text);
}

void sfText_getGlobalBounds_wrapper(const sfText* text, sfFloatRect* bounds) {
    *bounds = sfText_getGlobalBounds(text);
}

