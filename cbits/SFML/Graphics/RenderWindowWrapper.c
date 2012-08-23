#include <SFML/Graphics/RenderWindowWrapper.h>

sfRenderWindow* sfRenderWindow_create_wrapper(const sfVideoMode* mode, const char* title, sfUint32 style, const sfContextSettings* settings) {
    return sfRenderWindow_create(*mode, title, style, settings);
}

void sfRenderWindow_getSettings_wrapper(const sfRenderWindow* renderWindow, sfContextSettings* settings) {
    *settings = sfRenderWindow_getSettings(renderWindow);
}

void sfRenderWindow_getPosition_wrapper(const sfRenderWindow* renderWindow, sfVector2i* position) {
    *position = sfRenderWindow_getPosition(renderWindow);
}

void sfRenderWindow_setPosition_wrapper(sfRenderWindow* renderWindow, const sfVector2i* position) {
    sfRenderWindow_setPosition(renderWindow, *position);
}

void sfRenderWindow_getSize_wrapper(const sfRenderWindow* renderWindow, sfVector2u* size) {
    *size = sfRenderWindow_getSize(renderWindow);
}

void sfRenderWindow_setSize_wrapper(sfRenderWindow* renderWindow, const sfVector2u* size) {
    sfRenderWindow_setSize(renderWindow, *size);
}

void sfRenderWindow_clear_wrapper(sfRenderWindow* renderWindow, const sfColor* color) {
    sfRenderWindow_clear(renderWindow, *color);
}

void sfRenderWindow_getViewport_wrapper(const sfRenderWindow* renderWindow, const sfView* view, sfIntRect* viewport) {
    *viewport = sfRenderWindow_getViewport(renderWindow, view);
}

void sfRenderWindow_convertCoords_wrapper(const sfRenderWindow* renderWindow, const sfVector2i* point, const sfView* targetView, sfVector2f* out) {
    *out = sfRenderWindow_convertCoords(renderWindow, *point, targetView);
}

