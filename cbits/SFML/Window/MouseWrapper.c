#include <SFML/Window//MouseWrapper.h>

void sfMouse_getPosition_wrapper(const sfWindow* relativeTo, sfVector2i* position) {
    *position = sfMouse_getPosition(relativeTo);
}

void sfMouse_setPosition_wrapper(const sfWindow* relativeTo, const sfVector2i* position) {
    sfMouse_setPosition(*position, relativeTo);
}

