#include <SFML/System/TimeWrapper.h>

float sfTime_asSeconds_wrapper(sfTime* time) {
    return sfTime_asSeconds(*time);
}

sfInt32 sfTime_asMilliseconds_wrapper(sfTime* time) {
    return sfTime_asMilliseconds(*time);
}

sfInt64 sfTime_asMicroseconds_wrapper(sfTime* time) {
    return sfTime_asMicroseconds(*time);
}

void sfSeconds_wrapper(float amount, sfTime* time) {
    *time = sfSeconds(amount);
}

void sfMilliseconds_wrapper(sfInt32 amount, sfTime* time) {
    *time = sfMilliseconds(amount);
}

void sfMicroseconds_wrapper(sfInt64 amount, sfTime* time) {
    *time = sfMicroseconds(amount);
}

