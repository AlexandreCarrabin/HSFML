#ifndef SFML_SOUNDRECORDER_WRAPPER_H
#define SFML_SOUNDRECORDER_WRAPPER_H

#include <SFML/Audio/SoundRecorder.h>
#include <SFML/Audio/SoundRecorder_stub.h>
#include <HsFFI.h>

sfSoundRecorder* sfSoundRecorder_create_wrapper(HsStablePtr data);

#endif // SFML_SOUNDRECORDER_WRAPPER_H
