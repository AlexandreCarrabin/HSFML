#ifndef SFML_TEXT_WRAPPER_H
#define SFML_TEXT_WRAPPER_H

#include <SFML/Graphics/Text.h>

void sfText_setPosition_wrapper(sfText* text, const sfVector2f* position);

void sfText_setScale_wrapper(sfText* text, const sfVector2f* scale);

void sfText_setOrigin_wrapper(sfText* text, const sfVector2f* origin);

void sfText_getPosition_wrapper(const sfText* text, sfVector2f* position);

void sfText_getScale_wrapper(const sfText* text, sfVector2f* scale);

void sfText_getOrigin_wrapper(const sfText* text, sfVector2f* origin);

void sfText_move_wrapper(sfText* text, const sfVector2f* offset);

void sfText_scale_wrapper(sfText* text, const sfVector2f* factors);

void sfText_getTransform_wrapper(const sfText* text, sfTransform* transform);

void sfText_getInverseTransform_wrapper(const sfText* text, sfTransform* transform);

void sfText_setColor_wrapper(sfText* text, const sfColor* color);

void sfText_getColor_wrapper(const sfText* text, sfColor* color);

void sfText_findCharacterPos_wrapper(const sfText* text, size_t index, sfVector2f* position);

void sfText_getLocalBounds_wrapper(const sfText* text, sfFloatRect* bounds);

void sfText_getGlobalBounds_wrapper(const sfText* text, sfFloatRect* bounds);

#endif // SFML_TEXT_WRAPPER_H
