#include <SFML/Audio/MusicWrapper.h>

void sfMusic_getDuration_wrapper(const sfMusic* music, sfTime* duration) {
    *duration = sfMusic_getDuration(music);
}

void sfMusic_getPlayingOffset_wrapper(const sfMusic* music, sfTime* timeOffset) {
    *timeOffset = sfMusic_getPlayingOffset(music);
}

void sfMusic_setPosition_wrapper(sfMusic* music, const sfVector3f* position) {
    sfMusic_setPosition(music, *position);
}

void sfMusic_setPlayingOffset_wrapper(sfMusic* music, const sfTime* timeOffset) {
    sfMusic_setPlayingOffset(music, *timeOffset);
}

void sfMusic_getPosition_wrapper(const sfMusic* music, sfVector3f* position) {
    *position = sfMusic_getPosition(music);
}

