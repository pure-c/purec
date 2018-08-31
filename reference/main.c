#include "runtime/purescript.h"
#include "Data.Maybe.h"
#include "Example.h"

// this would come from prelude and would be supplied by compiler
/* // XXX result could be 'static', thus saving us from reallocating block on every call */
/* const Data_Show_Show * Data_Maybe_showInt () { */
/* 	Data_Show_Show * value0 = GC_NEW(Data_Show_Show); */
/* 	value0->show = */
/* 		PURS_ANY_BLOCK_NEW((const purs_any_t * i) { */
/* 			const int * value0 = purs_any_get_int(i); */
/* 			if (value0 != NULL) { */
/* 				return PURS_ANY_STRING_NEW(afmt("%i", *value0)); */
/* 			} */
/* 			assert (0); // XXX what now? */
/* 		}); */
/* 	return value0; */
/* } */

/* const purs_any_t * konst_1(const purs_ctx_t * ctx, const purs_any_t * b) { */
/* 	const purs_any_t * a = purs_record_find_by_key(ctx, "a")->value; */
/* 	return b; */
/* } */

/* purs_captured_func_t konst(const purs_ctx_t * ctx, const purs_any_t * a) { */
/* 	const purs_record_t * _ctx = purs_record_add_multi(NULL, 1, "a", a); */
/* 	return (purs_captured_func_t) { .ctx = _ctx, .fun = konst_1 }; */
/* } */

/*

let
  hello = \x ->
    let
      s = "hello"
      go1 = \y ->
        let
          s2 = "world"
          go2 = \i ->
            let
              next = i + 1
            in go2 next
        in go2
    in go1
in hello

*/

PURS_FFI_FUNC_1(hello, x, {
	const purs_any_t * s1 = PURS_ANY_STRING_NEW(afmt("hello"));

	const __block purs_any_t * go = PURS_LAMBDA(y, {
		return purs_any_app(PURS_LAMBDA(z, {
			GC_gcollect();

			const purs_any_t * s2 = PURS_ANY_STRING_NEW(afmt("world"));

			const __block purs_any_t * go2 = PURS_LAMBDA(i, {
				printf("s1:%s s2:%s (i:%i, x:%s, y:%s)\n",
				    purs_any_get_string(s1)->data,
				    purs_any_get_string(s2)->data,
				    *purs_any_get_int(i),
				    purs_any_get_string(x)->data,
				    purs_any_get_string(y)->data);

				GC_gcollect();
				PURS_ANY_INT_NEW(1);

				printf("s1:%s s2:%s (i:%i, x:%s, y:%s)!!!\n",
				    purs_any_get_string(s1)->data,
				    purs_any_get_string(s2)->data,
				    *purs_any_get_int(i),
				    purs_any_get_string(x)->data,
				    purs_any_get_string(y)->data);

				const purs_any_t * next = PURS_ANY_INT_NEW(*purs_any_get_int(i) + 1);

				if (*purs_any_get_int(i) > 100) {
					return s1;
				} else {
					return purs_any_app(go2, next);
				}
			});

			managed_block_t * go2_block = purs_any_get_abs_block(go2);

			return go2;
		}), NULL);
	});

	return go;
})


const purs_any_t * get_r () {
	const purs_any_t * f = purs_any_app(hello$, PURS_ANY_STRING_NEW("x"));
	    GC_gcollect();
	    PURS_ANY_INT_NEW(42);
	    const purs_any_t * r = purs_any_app(f, PURS_ANY_STRING_NEW("y"));
	    GC_gcollect();
	    PURS_ANY_INT_NEW(88);
	return r;
}

int main () {

	const purs_any_t * r = get_r();

	while (1) {

	    const purs_any_t * g = purs_any_app(r, PURS_ANY_INT_NEW(1));

	    GC_gcollect();
	    PURS_ANY_INT_NEW(1);

	    /* const purs_any_t * g = purs_any_app(r, PURS_ANY_INT_NEW(1)); */
	    GC_gcollect();
	    PURS_ANY_INT_NEW(1);


	    printf("%s\n", purs_any_get_string(g)->data);

	    GC_gcollect();
	    GC_gcollect();
	    GC_gcollect();
	    GC_gcollect();
	    GC_gcollect();
	    PURS_ANY_INT_NEW(1);
	    PURS_ANY_INT_NEW(1);
	    PURS_ANY_INT_NEW(1);
	    PURS_ANY_INT_NEW(1);

	    GC_gcollect();
	    GC_gcollect();
	    GC_gcollect();

	    PURS_ANY_INT_NEW(1);
	    PURS_ANY_INT_NEW(1);
	    PURS_ANY_INT_NEW(1);
	    PURS_ANY_INT_NEW(1);
	    PURS_ANY_INT_NEW(1);
	    PURS_ANY_INT_NEW(1);

	    GC_gcollect();

	    PURS_ANY_INT_NEW(1);

	    printf("%s\n", purs_any_get_string(g)->data);
	    usleep(100);

	}
}
