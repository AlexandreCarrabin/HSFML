#include <SFML/Graphics/SpriteWrapper.h>

void sfSprite_setPosition_wrapper(sfSprite* sprite, const sfVector2f* position) {
    sfSprite_setPosition(sprite, *position);
}

void sfSprite_setScale_wrapper(sfSprite* sprite, const sfVector2f* scale) {
    sfSprite_setScale(sprite, *scale);
}

void sfSprite_setOrigin_wrapper(sfSprite* sprite, const sfVector2f* origin) {
    sfSprite_setOrigin(sprite, *origin);
}

void sfSprite_getPosition_wrapper(const sfSprite* sprite, sfVector2f* position) {
    *position = sfSprite_getPosition(sprite);
}

void sfSprite_getScale_wrapper(const sfSprite* sprite, sfVector2f* scale) {
    *scale = sfSprite_getScale(sprite);
}

void sfSprite_getOrigin_wrapper(const sfSprite* sprite, sfVector2f* origin) {
    *origin = sfSprite_getOrigin(sprite);
}

void sfSprite_move_wrapper(sfSprite* sprite, const sfVector2f* offset) {
    sfSprite_move(sprite, *offset);
}

void sfSprite_scale_wrapper(sfSprite* sprite, const sfVector2f* factors) {
    sfSprite_scale(sprite, *factors);
}

void sfSprite_getTransform_wrapper(const sfSprite* sprite, sfTransform* transform) {
    *transform = sfSprite_getTransform(sprite);
}

void sfSprite_getInverseTransform_wrapper(const sfSprite* sprite, sfTransform* transform) {
    *transform = sfSprite_getInverseTransform(sprite);
}

void sfSprite_setTextureRect_wrapper(sfSprite* sprite, const sfIntRect* rectangle) {
    sfSprite_setTextureRect(sprite, *rectangle);
}

void sfSprite_setColor_wrapper(sfSprite* sprite, const sfColor* color) {
    sfSprite_setColor(sprite, *color);
}

void sfSprite_getTextureRect_wrapper(const sfSprite* sprite, sfIntRect* rectangle) {
    *rectangle = sfSprite_getTextureRect(sprite);
}

void sfSprite_getColor_wrapper(const sfSprite* sprite, sfColor* color) {
    *color = sfSprite_getColor(sprite);
}

void sfSprite_getLocalBounds_wrapper(const sfSprite* sprite, sfFloatRect* rectangle) {
    *rectangle = sfSprite_getLocalBounds(sprite);
}

void sfSprite_getGlobalBounds_wrapper(const sfSprite* sprite, sfFloatRect* rectangle) {
    *rectangle = sfSprite_getGlobalBounds(sprite);
}

