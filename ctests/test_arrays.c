#include "runtime/purescript.h"


int test_empty_array () {
}

int test_arrays () {
	const purs_vec_t * v = purs_vec_new_va(0);
	int i = 0;
	ANY tmp;
	purs_vec_foreach(v, tmp, i) {}
	return 0;
}
