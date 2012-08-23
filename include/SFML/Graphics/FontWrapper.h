#ifndef SFML_FONT_WRAPPER_H
#define SFML_FONT_WRAPPER_H

#include <SFML/Graphics/Font.h>

void sfFont_getGlyph_wrapper(sfFont* font, sfUint32 codePoint, unsigned int characterSize, sfBool bold, sfGlyph* glyph);

#endif // SFML_FONT_WRAPPER_H
