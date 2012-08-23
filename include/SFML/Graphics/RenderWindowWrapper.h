#ifndef SFML_RENDERWINDOW_WRAPPER_H
#define SFML_RENDERWINDOW_WRAPPER_H

#include <SFML/Graphics/RenderWindow.h>

sfRenderWindow* sfRenderWindow_create_wrapper(const sfVideoMode* mode, const char* title, sfUint32 style, const sfContextSettings* settings);

void sfRenderWindow_getSettings_wrapper(const sfRenderWindow* renderWindow, sfContextSettings* settings);

void sfRenderWindow_getPosition_wrapper(const sfRenderWindow* renderWindow, sfVector2i* position);

void sfRenderWindow_setPosition_wrapper(sfRenderWindow* renderWindow, const sfVector2i* position);

void sfRenderWindow_getSize_wrapper(const sfRenderWindow* renderWindow, sfVector2u* size);

void sfRenderWindow_setSize_wrapper(sfRenderWindow* renderWindow, const sfVector2u* size);

void sfRenderWindow_clear_wrapper(sfRenderWindow* renderWindow, const sfColor* color);

void sfRenderWindow_getViewport_wrapper(const sfRenderWindow* renderWindow, const sfView* view, sfIntRect* viewport);

void sfRenderWindow_convertCoords_wrapper(const sfRenderWindow* renderWindow, const sfVector2i* point, const sfView* targetView, sfVector2f* out);

#endif // SFML_RENDERWINDOW_WRAPPER_H
