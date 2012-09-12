#include <SFML/Audio/ListenerWrapper.h>

void sfListener_setPosition_wrapper(const sfVector3f* position) {
    sfListener_setPosition(*position);
}

void sfListener_getPosition_wrapper(sfVector3f* position) {
    *position = sfListener_getPosition();
}

void sfListener_setDirection_wrapper(const sfVector3f* direction) {
    sfListener_setDirection(*direction);
}

void sfListener_getDirection_wrapper(sfVector3f* direction) {
    *direction = sfListener_getDirection();
}

