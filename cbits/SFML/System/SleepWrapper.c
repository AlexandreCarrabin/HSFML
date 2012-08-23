#include <SFML/System/SleepWrapper.h>

void sfSleep_wrapper(const sfTime* time) {
    sfSleep(*time);
}

