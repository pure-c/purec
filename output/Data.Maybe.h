#ifndef DATA_MAYBE_H
#define DATA_MAYBE_H

#include "runtime/purescript.h"
#include "Data.Show.h"

#define Data_Maybe_Nothing__Tag 1
#define Data_Maybe_Just__Tag 2

const purs_any_t * Data_Maybe_Nothing;
const purs_any_t * Data_Maybe_Just (purs_any_t * value0);
const Data_Show_Show * Data_Maybe_showMaybe (const Data_Show_Show * dictShow);

#endif // DATA_MAYBE_H
