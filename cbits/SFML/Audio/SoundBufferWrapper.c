#include <SFML/Audio/SoundBufferWrapper.h>

void sfSoundBuffer_getDuration_wrapper(const sfSoundBuffer* soundBuffer, sfTime* duration) {
    *duration = sfSoundBuffer_getDuration(soundBuffer);
}

