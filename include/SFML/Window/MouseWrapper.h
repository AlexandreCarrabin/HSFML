#ifndef SFML_MOUSE_WRAPPER_H
#define SFML_MOUSE_WRAPPER_H

#include <SFML/Window/Mouse.h>

void sfMouse_getPosition_wrapper(const sfWindow* relativeTo, sfVector2i* position);

void sfMouse_setPosition_wrapper(const sfWindow* relativeTo, const sfVector2i* position);

#endif // SFML_MOUSE_WRAPPER_H
