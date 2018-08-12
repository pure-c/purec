#include <gc/gc.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <uthash.h>
#include "Block.h"
#include "purescript_runtime.h"

void purs_uthash_free (void * ptr, void *x) {
	GC_free(ptr);
}

void * purs_uthash_malloc (size_t size) {
	return GC_MALLOC(size);
}

#define PURS_DECLARE_INT (v) { \
	.tag = FLOAT, \
	.value = { ._int = v } \
}

#define PURS_DECLARE_INT (v) { \
	.tag = INT, \
	.value = { .num_int = v } \
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
	/* if (a4 != NULL) { */
	/* 	printf("tag: %d, value: %d\n", a4->tag, a4->value); */
	/* } */
}

typedef struct purs_record purs_record_t;
struct purs_record {
	const char * label;
	const purs_any_t * value;
	UT_hash_handle hh;
};

/* struct my_struct { */
/*     const char *name;          /\* key *\/ */
/*     int id; */
/*     UT_hash_handle hh;         /\* makes this structure hashable *\/ */
/* }; */


/* int main(int argc, char *argv[]) { */
/*     const char *names[] = { "joe", "bob", "betty", NULL }; */
/*     struct my_struct *s, *tmp, *users = NULL; */

/*     for (int i = 0; names[i]; ++i) { */
/*         s = (struct my_struct *)malloc(sizeof *s); */
/*         s->name = names[i]; */
/*         s->id = i; */
/*         HASH_ADD_KEYPTR( hh, users, s->name, strlen(s->name), s ); */
/*     } */

void foo () {
}

/* void foo () { */
/* 	purs_record_t * recs = NULL; */
/* 	int n; */
/* 	purs_record_t * recss [50000]; */
/* 	for (n = 0; n < 50000; n++) { */
/* 		void * x = malloc(10000); */
/* 		/\* purs_record_t * rec0 = GC_NEW(purs_record_t); *\/ */
/* 		/\* recss[n] = malloc(1024); *\/ */
/* 		/\* recss[n] = GC_MALLOC(1024); *\/ */
/* 		/\* strcpy(rec0->name, "foobar"); *\/ */
/* 		/\* rec0->val = NULL; *\/ */
/* 		/\* HASH_ADD_STR(recs, name, rec0); *\/ */
/* 		usleep(100); */
/* 	} */

/*     /\* /\\* free the hash table contents *\\/ *\/ */
/* 	/\* purs_record_t * s, * tmp = NULL; *\/ */
/*     /\* HASH_ITER(hh, recs, s, tmp) { *\/ */
/*     /\*   HASH_DEL(recs, s); *\/ */
/*     /\*   GC_FREE(s); *\/ */
/*     /\* } *\/ */

/* 	printf("done here\n"); */

/* 	/\* for (n = 0; n < 50000; n++) { *\/ */
/* 	/\* 	free(recss[n]); *\/ */
/* 	/\* 	recss[n] = NULL; *\/ */
/* 	/\* } *\/ */
/* 	/\* *recss = NULL; *\/ */

/* } */

int main () {
	GC_INIT();

	purs_any_t * f0 = MANAGED_BLOCK((purs_any_t * a) {
		return purs_any_set_int(
			GC_NEW(purs_any_t),
			200
		);
	});

	return 0;
}
