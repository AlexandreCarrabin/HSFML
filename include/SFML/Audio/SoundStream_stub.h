#include <HsFFI.h>
#include <SFML/Audio/SoundStream.h>
#ifdef __cplusplus
extern "C" {
#endif
extern HsBool hs_sfSoundStream_getDataCallback(sfSoundStreamChunk* a1, HsStablePtr a2);
extern void hs_sfSoundStream_seekCallback(sfTime* a1, HsStablePtr a2);
#ifdef __cplusplus
}
#endif

