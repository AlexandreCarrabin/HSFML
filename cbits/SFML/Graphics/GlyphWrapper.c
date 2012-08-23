#include <SFML/Graphics/GlyphWrapper.h>

void sfGlyph_getBounds_wrapper(const sfGlyph* glyph, sfIntRect* bounds) {
    *bounds = glyph->bounds;
}

void sfGlyph_setBounds_wrapper(sfGlyph* glyph, const sfIntRect* bounds) {
    glyph->bounds = *bounds;
}

void sfGlyph_getTextureRect_wrapper(const sfGlyph* glyph, sfIntRect* textureRect) {
    *textureRect = glyph->textureRect;
}

void sfGlyph_setTextureRect_wrapper(sfGlyph* glyph, const sfIntRect* textureRect) {
    glyph->textureRect = *textureRect;
}

