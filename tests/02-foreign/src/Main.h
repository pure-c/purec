#ifndef MAIN_H
#define MAIN_H

#include "runtime/purescript.h"

struct buf {
	char * data; /* heap-allocated buffer */
	int size;
};

PURS_FFI_EXPORT(Main_newBuffer);
PURS_FFI_EXPORT(Main_bufferSize);
PURS_FFI_EXPORT(Main_bufferGrow);

#endif // MAIN_H
