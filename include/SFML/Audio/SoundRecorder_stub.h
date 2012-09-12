#include <HsFFI.h>
#include <SFML/Audio/SoundRecorder.h>
#ifdef __cplusplus
extern "C" {
#endif
extern sfBool hs_sfSoundRecorder_startCallback(HsStablePtr a);
extern sfBool hs_sfSoundRecorder_processCallback(const sfInt16* a1, size_t a2, HsStablePtr a3);
extern void hs_sfSoundRecorder_stopCallback(HsStablePtr a);
#ifdef __cplusplus
}
#endif

