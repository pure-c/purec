#include <gc/gc.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include "Block.h"
#include "purescript_runtime.h"

// -----------------------------------------------------------------------------

#define MANAGED_C_STRING(x) \
	purs_any_set_c_string( \
		GC_NEW(struct purs_any_t), \
		x \
	)

#define MANAGED_BLOCK(x) \
	purs_any_set_abs_block( \
		GC_NEW(struct purs_any_t), \
		managed_block_new(Block_copy(^ x)) \
	)

struct purs_any_t * maybe(struct purs_any_t * b) {
	return MANAGED_BLOCK((struct purs_any_t * f) {
		return MANAGED_BLOCK((struct purs_any_t * mA) {
			struct purs_cons_t * cons = purs_any_get_cons (mA);
			printf("cons tag: %d\n", cons->tag);
			switch (cons->tag) {
				case _DATA_MAYBE__MAYBE__JUST: {
					return mA;
					/* return (struct purs_any_t *) NULL; */
					/* return b; */
				}
				case _DATA_MAYBE__MAYBE__NOTHING: {
					return b;
				}
			}
			return (struct purs_any_t *) NULL;
		});
	});
}

void main_1 () {

	struct purs_any_t * a1 = maybe(NULL);
	struct managed_block_t * b1 = purs_any_get_abs_block(a1);

	struct purs_any_t * a2 = ((abs_block_t)b1->block)(NULL);
	struct managed_block_t * b2 = purs_any_get_abs_block(a2);

	struct purs_cons_t * x0 = GC_NEW(struct purs_cons_t);
	struct purs_any_t * x1 = purs_any_set_cons(
		GC_NEW(struct purs_any_t),
		purs_cons_set(
			GC_NEW(struct purs_cons_t),
			_DATA_MAYBE__MAYBE__JUST,
			purs_any_set_int(
				GC_NEW(struct purs_any_t),
				100
			)
		)
	);

	// TODO: construct a real `Maybe` and pass in
	struct purs_any_t * a4 = ((abs_block_t)b2->block)(x1);
	printf("tag: %p\n", a4);
	printf("tag: %d, value: %d\n", a4->tag, purs_any_get_int(purs_any_get_cons(a4)->value));
}

int main () {
	GC_INIT();
	while (1) {
		main_1();
		usleep(100);
	}
	return 0;
}
