#ifndef SFML_VIEW_WRAPPER_H
#define SFML_VIEW_WRAPPER_H

#include <SFML/Graphics/View.h>

sfView* sfView_createFromRect_wrapper(const sfFloatRect* rectangle);

void sfView_setCenter_wrapper(sfView* view, const sfVector2f* center);

void sfView_setSize_wrapper(sfView* view, const sfVector2f* size);

void sfView_setViewport_wrapper(sfView* view, const sfFloatRect* viewport);

void sfView_reset_wrapper(sfView* view, const sfFloatRect* rectangle);

void sfView_getCenter_wrapper(const sfView* view, sfVector2f* center);

void sfView_getSize_wrapper(const sfView* view, sfVector2f* size);

void sfView_getViewport_wrapper(const sfView* view, sfFloatRect* viewport);

void sfView_move_wrapper(sfView* view, const sfVector2f* offset);

#endif // SFML_VIEW_WRAPPER_H
