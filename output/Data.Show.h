#ifndef DATA_SHOW_H
#define DATA_SHOW_H

#include "runtime/purescript.h"

typedef struct {
	purs_any_t * show;
} Data_Show_Show;

purs_any_t * Data_Show_show (Data_Show_Show * dict);

#endif // DATA_SHOW_H
