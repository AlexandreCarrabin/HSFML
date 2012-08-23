#ifndef SFML_WINDOW_WRAPPER_H
#define SFML_WINDOW_WRAPPER_H

#include <SFML/Window/Window.h>

sfWindow* sfWindow_create_wrapper(const sfVideoMode* mode, const char* title, sfUint32 style, const sfContextSettings* settings);

void sfWindow_getSettings_wrapper(const sfWindow* window, sfContextSettings* settings);

void sfWindow_getPosition_wrapper(const sfWindow* window, sfVector2i* position);

void sfWindow_setPosition_wrapper(sfWindow* window, const sfVector2i* position);

void sfWindow_getSize_wrapper(const sfWindow* window, sfVector2u* size);

void sfWindow_setSize_wrapper(sfWindow* window, const sfVector2u* size);

#endif // SFML_WINDOW_WRAPPER_H
