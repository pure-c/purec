#include "runtime/purescript.h"
#include "Data.Maybe.h"
#include "Example.h"

// this would come from prelude and would be supplied by compiler
// XXX result could be 'static', thus saving us from reallocating block on every call
const Data_Show_Show * Data_Maybe_showInt () {
	Data_Show_Show * value0 = GC_NEW(Data_Show_Show);
	value0->show =
		PURS_ANY_BLOCK((const purs_any_t * i) {
			const int * value0 = purs_any_get_int(i);
			if (value0 != NULL) {
				return purs_any_set_string(
					GC_NEW(purs_any_t),
					managed_utf8str_new(afmt("%i", *value0))
				);
			}
			assert (0); // XXX what now?
		});
	return value0;
}

int main () {
	const purs_any_t * a = Data_Maybe_Just(foo);
	const Data_Show_Show * x = Data_Maybe_showMaybe(Data_Maybe_showInt());
	const purs_any_t * y = purs_any_app(x->show, a);
	printf("%s\n", (char *) y->value.string->data);

	const purs_any_t * r = PURS_ANY_RECORD(1, "foobar", PURS_ANY_INT(200));
	const purs_record_t * r2 = purs_any_get_record(r);

	const purs_record_t * o = purs_record_find_by_key(r2, (void *) "foobar");
	printf("%d\n", *purs_any_get_int(o->value));

	const purs_record_t * r3 = purs_record_remove(r2, "foobar");

	const purs_record_t * o2 = purs_record_find_by_key(r2, (void *) "foobar");
	assert(o2 != NULL);
	printf("%d\n", *purs_any_get_int(o2->value));

	const purs_record_t * o3 = purs_record_find_by_key(r3, (void *) "foobar");
	assert(o3 == NULL);

	return 0;
}
