#include <purescript.h>

PURS_FFI_FUNC_3(Main_merge, _, _l, _r, {
	const purs_record_t * l = purs_any_get_record(_l);
	const purs_record_t * r = purs_any_get_record(_r);
	return purs_any_record_new(purs_record_merge(l, r));
});
