#ifndef SFML_SOUND_WRAPPER_H
#define SFML_SOUND_WRAPPER_H

#include <SFML/Audio/Sound.h>

void sfSound_setPosition_wrapper(sfSound* sound, const sfVector3f* position);

void sfSound_setPlayingOffset_wrapper(sfSound* sound, const sfTime* timeOffset);

void sfSound_getPosition_wrapper(const sfSound* sound, sfVector3f* position);

void sfSound_getPlayingOffset_wrapper(const sfSound* sound, sfTime* timeOffset);

#endif // SFML_SOUND_WRAPPER_H

