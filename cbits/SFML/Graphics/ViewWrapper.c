#include <SFML/Graphics/ViewWrapper.h>

sfView* sfView_createFromRect_wrapper(const sfFloatRect* rectangle) {
    return sfView_createFromRect(*rectangle);
}

void sfView_setCenter_wrapper(sfView* view, const sfVector2f* center) {
    sfView_setCenter(view, *center);
}

void sfView_setSize_wrapper(sfView* view, const sfVector2f* size) {
    sfView_setSize(view, *size);
}

void sfView_setViewport_wrapper(sfView* view, const sfFloatRect* viewport) {
    sfView_setViewport(view, *viewport);
}

void sfView_reset_wrapper(sfView* view, const sfFloatRect* rectangle) {
    sfView_reset(view, *rectangle);
}

void sfView_getCenter_wrapper(const sfView* view, sfVector2f* center) {
    *center = sfView_getCenter(view);
}

void sfView_getSize_wrapper(const sfView* view, sfVector2f* size) {
    *size = sfView_getSize(view);
}

void sfView_getViewport_wrapper(const sfView* view, sfFloatRect* viewport) {
    *viewport = sfView_getViewport(view);
}

void sfView_move_wrapper(sfView* view, const sfVector2f* offset) {
    sfView_move(view, *offset);
}

