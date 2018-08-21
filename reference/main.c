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

typedef struct purs_record {
    purs_any_t * key;
    purs_any_t * value;
    UT_hash_handle hh;
} purs_record_t;


#define PURS_RECORD_ADD_MUT($record, $key, $value) \
	do { \
		purs_record_t * entry = GC_NEW(purs_record_t); \
		entry->key = PURS_ANY_STRING($key); \
		entry->value = $value; \
		HASH_ADD_PTR($record, key, entry); \
	} while (0)

const purs_record_t ** purs_record_copy(const purs_record_t **) {

}

const purs_record_t ** make_record_foo () {
	purs_record_t ** record = GC_NEW(purs_record_t *);
	PURS_RECORD_ADD_MUT(*record, "foobar", PURS_ANY_INT(100));
	return (const purs_record_t **) record;
}

// XXX: do this properly in amortized O(1)
const purs_any_t * purs_record_find_by_key(const purs_record_t ** record, void * key) {
	const purs_record_t * current_entry, * tmp;
	HASH_ITER(hh, *record, current_entry, tmp) {
		if (purs_any_eq_string(current_entry->key, key)) {
			return current_entry->value;
		}
	}
	return NULL;
}

int main () {
	const purs_any_t * a = Data_Maybe_Just(foo);
	const Data_Show_Show * x = Data_Maybe_showMaybe(Data_Maybe_showInt());
	const purs_any_t * y = purs_any_app(x->show, a);
	printf("%s\n", (char *) y->value.string->data);

	const purs_record_t ** r = make_record_foo();
	const purs_any_t * o = purs_record_find_by_key(r, (void *) "foobar");
	printf("%d\n", *purs_any_get_int(o));

	return 0;
}
