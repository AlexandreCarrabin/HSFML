#ifndef SFML_MUSIC_WRAPPER_H
#define SFML_MUSIC_WRAPPER_H

#include <SFML/Audio/Music.h>

void sfMusic_getDuration_wrapper(const sfMusic* music, sfTime* duration);

void sfMusic_getPlayingOffset_wrapper(const sfMusic* music, sfTime* timeOffset);

void sfMusic_setPosition_wrapper(sfMusic* music, const sfVector3f* position);

void sfMusic_setPlayingOffset_wrapper(sfMusic* music, const sfTime* timeOffset);

void sfMusic_getPosition_wrapper(const sfMusic* music, sfVector3f* position);

#endif // SFML_MUSIC_WRAPPER_H

