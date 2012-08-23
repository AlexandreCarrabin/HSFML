#include <SFML/Window/WindowWrapper.h>

sfWindow* sfWindow_create_wrapper(const sfVideoMode* mode, const char* title, sfUint32 style, const sfContextSettings* settings) {
    return sfWindow_create(*mode, title, style, settings);
}

void sfWindow_getSettings_wrapper(const sfWindow* window, sfContextSettings* settings) {
    *settings = sfWindow_getSettings(window);
}

void sfWindow_getPosition_wrapper(const sfWindow* window, sfVector2i* position) {
    *position = sfWindow_getPosition(window);
}

void sfWindow_setPosition_wrapper(sfWindow* window, const sfVector2i* position) {
    sfWindow_setPosition(window, *position);
}

void sfWindow_getSize_wrapper(const sfWindow* window, sfVector2u* size) {
    *size = sfWindow_getSize(window);
}

void sfWindow_setSize_wrapper(sfWindow* window, const sfVector2u* size) {
    sfWindow_setSize(window, *size);
}

