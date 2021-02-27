#undef UNIT_TESTING
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#define UNIT_TESTING
#include "runtime/purescript.h"

static ANY mk_prefix_cont_0(const struct purs_scope * scope, ANY arg, va_list _) {
	const char * prefix = purs_any_unsafe_get_string(scope->bindings[0])->data;
	const char * suffix = purs_any_unsafe_get_string(arg)->data;
	return purs_any_string(purs_str_new("%s%s", prefix, suffix));
}

static ANY mk_prefix_cont (const char * prefix) {
	const purs_str_t * s = purs_str_new("%s", prefix);
	const struct purs_scope * scope = ({
		const purs_any_t x = purs_any_string(s);
		purs_scope_new(1, x);
	});
	const purs_cont_t * cont = purs_cont_new(scope, mk_prefix_cont_0);
	PURS_RC_RELEASE(scope);
	PURS_RC_RELEASE(s);
	return purs_any_cont(cont);
}

static void leak_cont_test(void **state) {
	(void) state; /* unused */
	ANY cont = mk_prefix_cont("foo: ");
	const purs_str_t * s = purs_str_new("bar");
	ANY output = purs_any_app(cont, purs_any_string(s));
	PURS_ANY_RELEASE(output);
	output = purs_any_app(cont, purs_any_string(s));
	PURS_ANY_RELEASE(output);
	PURS_RC_RELEASE(s);
	PURS_ANY_RELEASE(cont);
}

static void purs_scope_new1_test(void **state) {
	(void) state; /* unused */
	const purs_scope_t * s = purs_scope_new1(1);
	const purs_cont_t * c = purs_cont_new(s, NULL);
	PURS_RC_RELEASE(s);
	PURS_RC_RELEASE(c);
}

static void leak_string_test(void **state) {
	(void) state; /* unused */
	const purs_str_t * s = purs_str_new("bar");
	PURS_RC_RELEASE(s);
}

/* todo: test empty array */
static void leak_array_test(void **state) {
	(void) state; /* unused */
	const purs_str_t * s1 = purs_str_new("foo");
	const purs_str_t * s2 = purs_str_new("bar");
	const purs_vec_t * v1 = purs_vec_new_va(1, purs_any_string(s1));
	const purs_vec_t * v2 = purs_vec_copy(v1);
	const purs_vec_t * v3 = purs_vec_splice(v2, 0, 0);
	PURS_RC_RELEASE(s1);
	PURS_RC_RELEASE(v1);
	assert_string_equal(s1->data, "foo"); /* should not seg-fault */
	PURS_RC_RELEASE(v2);
	const purs_vec_t * v4 = purs_vec_insert(v3, 0, purs_any_string(s2));
	const purs_vec_t * v5 = purs_vec_concat(v4, v3);
	PURS_RC_RELEASE(s2);
	PURS_RC_RELEASE(v3);
	PURS_RC_RELEASE(v4);

	/* test: concat */
	assert_null(purs_vec_concat(NULL, NULL));

	const purs_vec_t * tmp;
	assert_ptr_equal(tmp = purs_vec_concat(NULL, v5), v5);
	PURS_RC_RELEASE(tmp);

	assert_ptr_equal(tmp = purs_vec_concat(v5, NULL), v5);
	PURS_RC_RELEASE(tmp);

	PURS_RC_RELEASE(v5);
}

static void purs_vec_concat_test(void **state) {
	(void) state; /* unused */

	ANY s1 = purs_any_string(purs_str_new("a"));
	ANY s2 = purs_any_string(purs_str_new("b"));
	ANY s3 = purs_any_string(purs_str_new("c"));

	const purs_vec_t * v1 = purs_vec_new_va(1, s1);
	const purs_vec_t * v2 = purs_vec_new_va(2, s2, s3);
	const purs_vec_t * v3 = purs_vec_concat(v1, v2);

	PURS_ANY_RELEASE(s1);
	PURS_ANY_RELEASE(s2);
	PURS_ANY_RELEASE(s3);

	assert_int_equal(v1->length, 1);
	assert_int_equal(v2->length, 2);
	assert_int_equal(v3->length, 3);

	assert_string_equal(purs_any_unsafe_get_string(v1->data[0])->data, "a");
	assert_string_equal(purs_any_unsafe_get_string(v2->data[0])->data, "b");
	assert_string_equal(purs_any_unsafe_get_string(v2->data[1])->data, "c");
	assert_string_equal(purs_any_unsafe_get_string(v3->data[0])->data, "a");
	assert_string_equal(purs_any_unsafe_get_string(v3->data[1])->data, "b");
	assert_string_equal(purs_any_unsafe_get_string(v3->data[2])->data, "c");

	PURS_RC_RELEASE(v1);
	PURS_RC_RELEASE(v2);
	PURS_RC_RELEASE(v3);
}

/* todo: test empty record */
static void leak_record_test(void **state) {
	(void) state; /* unused */
	const purs_str_t *k1 = purs_str_static_new("s1");
	const purs_str_t *k2 = purs_str_static_new("s2");
	const purs_str_t *k3 = purs_str_static_new("s3");
	const purs_str_t *s1 = purs_str_static_new("foo");
	const purs_str_t *s2 = purs_str_static_new("bar");
	const purs_record_t *x = purs_record_new_va(
		2,
		k1, purs_any_string(s1),
		k2, purs_any_string(s2));
	assert_string_equal("foo", s1->data); /* should not seg-fault */
	assert_string_equal("bar", s2->data); /* should not seg-fault */
	PURS_RC_RELEASE(k1);
	PURS_RC_RELEASE(k2);
	PURS_RC_RELEASE(s1);
	PURS_RC_RELEASE(s2);
	const purs_record_t *x2 = purs_record_copy_shallow(x);
	assert_string_equal("foo", s1->data); /* should not seg-fault */
	assert_string_equal("bar", s2->data); /* should not seg-fault */
	const purs_record_t *x3 =
		purs_record_add_multi(x2,
				      2,
				      k3, purs_any_string(s1),
				      k3, purs_any_string(s1));
	PURS_RC_RELEASE(x);
	assert_string_equal("foo", s1->data); /* should not seg-fault */
	assert_string_equal("bar", s2->data); /* should not seg-fault */
	PURS_RC_RELEASE(x2);
	assert_string_equal("foo", s1->data); /* should not seg-fault */
	assert_string_equal("bar", s2->data); /* should not seg-fault */

	/* test key lookup. note looking up on a released record seg-faults! */
	ANY r = *purs_record_find_by_key(x3, k3);
	assert(strcmp("foo", purs_any_unsafe_get_string(r)->data) == 0); /* should not seg-fault */

	PURS_RC_RELEASE(x3);
	PURS_RC_RELEASE(k3);
}


static void purs_any_concat_test(void **state) {
	(void) state; /* unused */

	/* test: cannot concat nums */
	expect_assert_failure(purs_any_concat(purs_any_int(0),
					      purs_any_num(0)));

	/* test: cannot concat ints */
	expect_assert_failure(purs_any_concat(purs_any_int(0.0),
					      purs_any_num(0.0)));

	/* test: cannot concat differing types */
	for (int i = 0; i < PURS_ANY_TAGS_TOT; i++) {
		int j;
		if (i == 0) j = PURS_ANY_TAGS_TOT;
		else j = i + 1;
		if (i == PURS_ANY_TAG_THUNK || j == PURS_ANY_TAG_THUNK) {
			/* skip thunks, for now. */
			continue;
		}
		expect_assert_failure(purs_any_concat((ANY) { .tag = i },
						      (ANY) { .tag = j }));
	}

	/* test: concat strings */
	{
		ANY a = purs_any_string(purs_str_new("a"));
		ANY b = purs_any_string(purs_str_new("b"));
		ANY ab = purs_any_concat(a, b);
		PURS_ANY_RELEASE(b);
		PURS_ANY_RELEASE(a);
		assert_string_equal(purs_any_unsafe_get_string(ab)->data, "ab");
		PURS_ANY_RELEASE(ab);
	}

	/* test: arrays */
	{
		ANY a = purs_any_string(purs_str_new("a"));
		ANY b = purs_any_string(purs_str_new("b"));
		ANY c = purs_any_string(purs_str_new("c"));
		const purs_vec_t * v1 = purs_vec_new_va(2, a, b);
		const purs_vec_t * v2 = purs_vec_new_va(3, b, a, c);
		ANY v1v2 = purs_any_concat(purs_any_array(v1),
					   purs_any_array(v2));
		PURS_ANY_RELEASE(a);
		PURS_ANY_RELEASE(b);
		PURS_ANY_RELEASE(c);
		PURS_RC_RELEASE(v1);
		PURS_RC_RELEASE(v2);
		assert_string_equal(purs_any_unsafe_get_string(a)->data, "a"); /* should not seg-fault */
		assert_string_equal(purs_any_unsafe_get_string(b)->data, "b"); /* should not seg-fault */
		assert_string_equal(purs_any_unsafe_get_string(c)->data, "c"); /* should not seg-fault */
		PURS_ANY_RELEASE(v1v2);
	}
}

static void purs_any_lazy_test(void **state) {
	(void) state; /* unused */

	{ /* unassigned/null */
		ANY * ivalue = purs_any_ref_new();
		ANY ithunk = purs_any_lazy_new(ivalue);
		int has_changed;
		ANY val = purs_any_unthunk(ithunk, &has_changed);
		assert_int_equal(has_changed, 1);
		assert_int_equal(val.tag, PURS_ANY_TAG_NULL);
		PURS_ANY_RELEASE(ithunk);
	}

	{ /* retained results */

		ANY arr;
		{ /* allocate array */
			const purs_str_t * str = purs_str_new("fooba");
			arr = purs_any_array(purs_vec_new_va(1, purs_any_string(str)));
			PURS_RC_RELEASE(str);
		}

		ANY ithunk;
		{ /* create and fill thunk */
			ANY * ivalue = purs_any_ref_new();
			ithunk = purs_any_lazy_new(ivalue /* move */);
			purs_any_ref_write(ivalue, arr);
		}

		{ /* force the thunk */
			int has_changed;
			ANY val = purs_any_unthunk(ithunk, &has_changed);
			assert_int_equal(has_changed, 1);
			assert_int_equal(val.tag, PURS_ANY_TAG_ARRAY);
			PURS_ANY_RELEASE(val);
		}

		PURS_ANY_RELEASE(arr);
		PURS_ANY_RELEASE(ithunk);
	}
}

static void purs_cons_test(void **state) {
	(void) state; /* unused */

	{ /* empty */
		const purs_cons_t * cons = purs_cons_new(1, 0, NULL);
		assert_null(cons->values);
		PURS_RC_RELEASE(cons);
	}

	{ /* non-empty */
		const purs_vec_t * vec = purs_vec_new_va(1, purs_any_int(1));
		const purs_cons_t * cons = purs_cons_new(1, 1, purs_any_array(vec));
		PURS_RC_RELEASE(vec);
		PURS_RC_RELEASE(cons);
	}

	{ /* non-empty, wrapped */
		const purs_vec_t * vec = purs_vec_new_va(1, purs_any_int(1));
		ANY cons = purs_any_cons(purs_cons_new(1, 1, purs_any_array(vec)));
		PURS_RC_RELEASE(vec);
		PURS_ANY_RELEASE(cons);
	}
}

static void purs_any_ref_test(void **state) {
	(void) state; /* unused */
	ANY *ivalue = purs_any_ref_new();
	const purs_cont_t *cont = purs_cont_new(NULL, NULL);
	purs_any_ref_write(ivalue, purs_any_cont(cont));
	PURS_RC_RELEASE(cont);
	purs_any_ref_free(ivalue);
}

static void purs_string_test(void **state) {
	(void) state; /* unused */
	const purs_str_t *x = purs_str_new("test");
	ANY y = purs_any_string(x);
	const purs_str_t *z = purs_any_force_string(y);
	assert_string_equal("test", z->data);
	PURS_RC_RELEASE(z);
	assert_string_equal("test", x->data);
	PURS_RC_RELEASE(x);
}

int main (void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(leak_string_test),
		cmocka_unit_test(leak_array_test),
		cmocka_unit_test(leak_record_test),
		cmocka_unit_test(leak_cont_test),
		cmocka_unit_test(purs_scope_new1_test),
		cmocka_unit_test(purs_any_concat_test),
		cmocka_unit_test(purs_vec_concat_test),
		cmocka_unit_test(purs_any_ref_test),
		cmocka_unit_test(purs_any_lazy_test),
		cmocka_unit_test(purs_cons_test),
		cmocka_unit_test(purs_string_test),
	};

	return cmocka_run_group_tests(tests, NULL, NULL);
}
