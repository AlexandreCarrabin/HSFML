#ifndef SFML_INPUTSTREAM_WRAPPER_H
#define SFML_INPUTSTREAM_WRAPPER_H

#include <SFML/System/InputStream.h>
#include <SFML/System/InputStream_stub.h>
#include <HsFFI.h>

sfInputStream* sfInputStream_create_wrapper(HsStablePtr data);

void sfInputStream_destroy_wrapper(sfInputStream* inputStream);

#endif // SFML_INPUTSTREAM_WRAPPER_H
