#include <SFML/Audio/SoundWrapper.h>

void sfSound_setPosition_wrapper(sfSound* sound, const sfVector3f* position) {
    sfSound_setPosition(sound, *position);
}

void sfSound_setPlayingOffset_wrapper(sfSound* sound, const sfTime* timeOffset) {
    sfSound_setPlayingOffset(sound, *timeOffset);
}

void sfSound_getPosition_wrapper(const sfSound* sound, sfVector3f* position) {
    *position = sfSound_getPosition(sound);
}

void sfSound_getPlayingOffset_wrapper(const sfSound* sound, sfTime* timeOffset) {
    *timeOffset = sfSound_getPlayingOffset(sound);
}

