#include <SFML/Window/VideoModeWrapper.h>

void sfVideoMode_getDesktopMode_wrapper(sfVideoMode* mode) {
    *mode = sfVideoMode_getDesktopMode();
}

sfBool sfVideoMode_isValid_wrapper(sfVideoMode* mode) {
    return sfVideoMode_isValid(*mode);
}

