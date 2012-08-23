#ifndef SFML_CLOCK_WRAPPER_H
#define SFML_CLOCK_WRAPPER_H

#include <SFML/System/Clock.h>

void sfClock_getElapsedTime_wrapper(const sfClock* clock, sfTime* time);

void sfClock_restart_wrapper(sfClock* clock, sfTime* time);

#endif // SFML_CLOCK_WRAPPER_H
