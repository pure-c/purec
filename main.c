#include <gc/gc.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <uthash.h>
#include "Block.h"
#include "purescript_runtime.h"

void freer(void * ptr, void * x) {
  GC_free(ptr);
}

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

typedef struct purs_record purs_record_t;
struct purs_record {
  char name[255];
  purs_any_t * val;
  UT_hash_handle hh;
};

void foo () {
	purs_record_t * rec = NULL;
	int n = 0;
	while (n < 50000) {
		n++;
		purs_record_t * rec0 = GC_NEW(purs_record_t);
		/* strcpy(rec0->name, "foobar"); */
		/* rec0->val = NULL; */
		HASH_ADD_STR(rec, name, rec0);
		usleep(100);
	}
	printf("done here\n");
}

int main () {
	GC_INIT();

	/* purs_any_t * f0 = MANAGED_BLOCK((purs_any_t * a) { */
	/* 	return purs_any_set_int( */
	/* 		GC_NEW(purs_any_t), */
	/* 		200 */
	/* 	); */
	/* }); */

	/* main_1(f0); */
	foo();

	GC_gcollect();

	while (1) {
		usleep(10000);
	}

	return 0;
}
