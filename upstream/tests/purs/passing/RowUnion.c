#include <purescript.h>

PURS_FFI_FUNC_3(Main_merge, _, _l, _r) {
	const purs_record_t *l = purs_any_force_record(_l);
	const purs_record_t *r = purs_any_force_record(_r);
	purs_any_t ret = purs_any_record(purs_record_merge(l, r));
	PURS_RC_RELEASE(l);
	PURS_RC_RELEASE(r);
	return ret;
}
