#include <HsFFI.h>
#ifdef __cplusplus
extern "C" {
#endif
extern HsInt64 hs_readCallback(HsPtr a1, HsInt64 a2, HsStablePtr a3);
extern HsInt64 hs_seekCallback(HsInt64 a1, HsStablePtr a2);
extern HsInt64 hs_tellCallback(HsStablePtr a1);
extern HsInt64 hs_getSizeCallback(HsStablePtr a1);
#ifdef __cplusplus
}
#endif

