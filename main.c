#include <gc/gc.h>
#include <unistd.h>
#include <stdio.h>
#include "Block.h"
#include "purescript_runtime.h"

/* #define MANAGED_BLOCK_CALL(x, arg) \ */
/* 	(struct purs_any_t *)(((purs_abs) x->block)(arg)); */



// -----------------------------------------------------------------------------

struct purs_any_t * go (char* x) {
	return purs_any_from_managed_block(managed_block_new(
		Block_copy(^ () {
			return purs_any_from_managed_block(managed_block_new(
				Block_copy(^ (int y) {
					printf ("%s: %d\n", x, y);
					return 0;
				})
			));
		})
	));
}

void main_1 () {
	struct purs_any_t * x0 = go ("foobar");
	/* struct purs_any_t * x1 = MANAGED_BLOCK_CALL(purs_any_get_managed_block(x0), 0); */
	/* struct purs_any_t * x2 = MANAGED_BLOCK_CALL(purs_any_get_managed_block(x1), 100); */
}

int main () {
	GC_INIT();
	/* while (1) { */
	/* 	main_1(); */
	/* 	usleep(100); */
	/* } */
	return 0;
}
