#include <SFML/Audio/SoundStreamWrapper.h>

sfBool hs_sfSoundStream_getDataCallback_wrapper(sfSoundStreamChunk* chunk, void* data) {
    hs_sfSoundStream_getDataCallback(chunk, data);
}

void hs_sfSoundStream_seekCallback_wrapper(sfTime time, void* data) {
    hs_sfSoundStream_seekCallback(&time, data);
}

sfSoundStream* sfSoundStream_create_wrapper(unsigned int channelCount, unsigned int sampleRate, HsStablePtr data) {
    return sfSoundStream_create(&hs_sfSoundStream_getDataCallback_wrapper, &hs_sfSoundStream_seekCallback_wrapper, channelCount, sampleRate, data);
}

void sfSoundStream_setPosition_wrapper(sfSoundStream* stream, const sfVector3f* position) {
    sfSoundStream_setPosition(stream, *position);
}

void sfSoundStream_setPlayingOffset_wrapper(sfSoundStream* stream, const sfTime* timeOffset) {
    sfSoundStream_setPlayingOffset(stream, *timeOffset);
}

void sfSoundStream_getPosition_wrapper(const sfSoundStream* stream, sfVector3f* position) {
    *position = sfSoundStream_getPosition(stream);
}

void sfSoundStream_getPlayingOffset_wrapper(const sfSoundStream* stream, sfTime* timeOffset) {
    *timeOffset = sfSoundStream_getPlayingOffset(stream);
}

