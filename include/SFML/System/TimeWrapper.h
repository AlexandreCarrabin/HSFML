#ifndef SFML_TIME_WRAPPER_H
#define SFML_TIME_WRAPPER_H

#include <SFML/System/Time.h>

float sfTime_asSeconds_wrapper(sfTime* time);

sfInt32 sfTime_asMilliseconds_wrapper(sfTime* time);

sfInt64 sfTime_asMicroseconds_wrapper(sfTime* time);

void sfSeconds_wrapper(float amount, sfTime* time);

void sfMilliseconds_wrapper(sfInt32 amount, sfTime* time);

void sfMicroseconds_wrapper(sfInt64 amount, sfTime* time);

#endif // SFML_TIME_WRAPPER_H
