#ifndef SFML_GLYPH_WRAPPER_H
#define SFML_GLYPH_WRAPPER_H

#include <SFML/Graphics/Glyph.h>

void sfGlyph_getBounds_wrapper(const sfGlyph* glyph, sfIntRect* bounds);

void sfGlyph_setBounds_wrapper(sfGlyph* glyph, const sfIntRect* bounds);

void sfGlyph_getTextureRect_wrapper(const sfGlyph* glyph, sfIntRect* textureRect);

void sfGlyph_setTextureRect_wrapper(sfGlyph* glyph, const sfIntRect* textureRect);

#endif // SFML_GLYPH_WRAPPER_H
