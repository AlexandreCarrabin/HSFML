#include <SFML/Audio/SoundRecorderWrapper.h>

sfSoundRecorder* sfSoundRecorder_create_wrapper(HsStablePtr data) {
    return sfSoundRecorder_create(&hs_sfSoundRecorder_startCallback, &hs_sfSoundRecorder_processCallback, &hs_sfSoundRecorder_stopCallback, data);
}

