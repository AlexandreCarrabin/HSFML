#include <SFML/System/InputStreamWrapper.h>
#include <stdlib.h>

sfInputStream* sfInputStream_create_wrapper(HsStablePtr data) {
    sfInputStream* inputStream = (sfInputStream*)malloc(sizeof(sfInputStream));

    inputStream->read     = hs_readCallback;
    inputStream->seek     = hs_seekCallback;
    inputStream->tell     = hs_tellCallback;
    inputStream->getSize  = hs_getSizeCallback;
    inputStream->userData = data;

    return inputStream;
}

void sfInputStream_destroy_wrapper(sfInputStream* inputStream) {
    free(inputStream);
}

