#include <SFML/System/ClockWrapper.h>

void sfClock_getElapsedTime_wrapper(const sfClock* clock, sfTime* time) {
    *time = sfClock_getElapsedTime(clock);
}

void sfClock_restart_wrapper(sfClock* clock, sfTime* time) {
    *time = sfClock_restart(clock);
}

