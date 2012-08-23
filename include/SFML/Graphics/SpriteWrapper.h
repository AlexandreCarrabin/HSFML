#ifndef SFML_SPRITE_WRAPPER_H
#define SFML_SPRITE_WRAPPER_H

#include <SFML/Graphics/Sprite.h>

void sfSprite_setPosition_wrapper(sfSprite* sprite, const sfVector2f* position);

void sfSprite_setScale_wrapper(sfSprite* sprite, const sfVector2f* scale);

void sfSprite_getOrigin_wrapper(const sfSprite* sprite, sfVector2f* origin);

void sfSprite_getPosition_wrapper(const sfSprite* sprite, sfVector2f* position);

void sfSprite_getScale_wrapper(const sfSprite* sprite, sfVector2f* scale);

void sfSprite_setOrigin_wrapper(sfSprite* sprite, const sfVector2f* origin);

void sfSprite_move_wrapper(sfSprite* sprite, const sfVector2f* offset);

void sfSprite_scale_wrapper(sfSprite* sprite, const sfVector2f* factors);

void sfSprite_getTransform_wrapper(const sfSprite* sprite, sfTransform* transform);

void sfSprite_getInverseTransform_wrapper(const sfSprite* sprite, sfTransform* transform);

void sfSprite_setTextureRect_wrapper(sfSprite* sprite, const sfIntRect* rectangle);

void sfSprite_setColor_wrapper(sfSprite* sprite, const sfColor* color);

void sfSprite_getTextureRect_wrapper(const sfSprite* sprite, sfIntRect* rectangle);

void sfSprite_getColor_wrapper(const sfSprite* sprite, sfColor* color);

void sfSprite_getLocalBounds_wrapper(const sfSprite* sprite, sfFloatRect* rectangle);

void sfSprite_getGlobalBounds_wrapper(const sfSprite* sprite, sfFloatRect* rectangle);

#endif // SFML_SPRITE_WRAPPER_H
