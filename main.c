#include <gc/gc.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <uthash.h>
#include "Block.h"
#include "purescript_runtime.h"

// -----------------------------------------------------------------------------

#define MANAGED_C_STRING(x) \
	purs_any_set_c_string( \
		GC_NEW(purs_any_t), \
		x \
	)

#define MANAGED_BLOCK(x) \
	purs_any_set_abs_block( \
		GC_NEW(purs_any_t), \
		managed_block_new(Block_copy(^ x)) \
	)

// EXAMPLE: Maybe a
// data Maybe a b = Just a | Nothing
#define _DATA_MAYBE__MAYBE__JUST 1
#define _DATA_MAYBE__MAYBE__NOTHING 2

purs_any_t * maybe(purs_any_t * b) {
	return MANAGED_BLOCK((purs_any_t * f) {
		return MANAGED_BLOCK((purs_any_t * mA) {
			purs_cons_t * cons = purs_any_get_cons (mA);
			switch (cons->tag) {
				case _DATA_MAYBE__MAYBE__JUST: {
					// TODO: account for `f` being an ABS (not ABS_BLOCK)
					managed_block_t * f0 = purs_any_get_abs_block(f);
					return (purs_any_t *)((abs_block_t)f0->block)(cons->value);
				}
				case _DATA_MAYBE__MAYBE__NOTHING: {
					return b;
				}
			}
			return (purs_any_t *) NULL;
		});
	});
}

void main_1 (purs_any_t * f0) {

	purs_any_t * a1 = maybe(NULL);
	managed_block_t * b1 = purs_any_get_abs_block(a1);

	purs_any_t * a2 = ((abs_block_t)b1->block)(f0);
	managed_block_t * b2 = purs_any_get_abs_block(a2);

	purs_cons_t * x0 = GC_NEW(purs_cons_t);
	purs_any_t * x1 = purs_any_set_cons(
		GC_NEW(purs_any_t),
		purs_cons_set(
			GC_NEW(purs_cons_t),
			_DATA_MAYBE__MAYBE__JUST,
			purs_any_set_int(
				GC_NEW(purs_any_t),
				100
			)
		)
	);

	// TODO: construct a real `Maybe` and pass in
	purs_any_t * a4 = ((abs_block_t)b2->block)(x1);
	printf("tag: %p\n", a4);
	if (a4 != NULL) {
		printf("tag: %d, value: %d\n", a4->tag, a4->value);
	}
}

int main () {
	GC_INIT();

	int * k = GC_MALLOC(100);
	*k = 100;

	purs_any_t * f0 = MANAGED_BLOCK((purs_any_t * a) {
		return purs_any_set_int(
			GC_NEW(purs_any_t),
			*k // 200
		);
	});

	while (1) {
		/* hcreate(30); */
		main_1(f0);
		usleep(100);
	}
	return 0;
}
