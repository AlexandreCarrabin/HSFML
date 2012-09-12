#ifndef SFML_SOUNDSTREAM_WRAPPER_H
#define SFML_SOUNDSTREAM_WRAPPER_H

#include <SFML/Audio/SoundStream.h>
#include <SFML/Audio/SoundStream_stub.h>
#include <HsFFI.h>

sfBool hs_sfSoundStream_getDataCallback_wrapper(sfSoundStreamChunk* chunk, void* data);

void hs_sfSoundStream_seekCallback_wrapper(sfTime time, void* data);

sfSoundStream* sfSoundStream_create_wrapper(unsigned int channelCount, unsigned int sampleRate, HsStablePtr data);

void sfSoundStream_setPosition_wrapper(sfSoundStream* stream, const sfVector3f* position);

void sfSoundStream_setPlayingOffset_wrapper(sfSoundStream* stream, const sfTime* timeOffset);

void sfSoundStream_getPosition_wrapper(const sfSoundStream* stream, sfVector3f* position);

void sfSoundStream_getPlayingOffset_wrapper(const sfSoundStream* stream, sfTime* timeOffset);

#endif // SFML_SOUNDSTREAM_WRAPPER_H
