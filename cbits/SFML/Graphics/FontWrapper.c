#include <SFML/Graphics/FontWrapper.h>

void sfFont_getGlyph_wrapper(sfFont* font, sfUint32 codePoint, unsigned int characterSize, sfBool bold, sfGlyph* glyph) {
    *glyph = sfFont_getGlyph(font, codePoint, characterSize, bold);
}

