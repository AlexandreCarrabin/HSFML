#ifndef SFML_LISTENER_WRAPPER_H
#define SFML_LISTENER_WRAPPER_H

#include <SFML/Audio/Listener.h>

void sfListener_setPosition_wrapper(const sfVector3f* position);

void sfListener_getPosition_wrapper(sfVector3f* position);

void sfListener_setDirection_wrapper(const sfVector3f* direction);

void sfListener_getDirection_wrapper(sfVector3f* direction);

#endif // SFML_LISTENER_WRAPPER_H
