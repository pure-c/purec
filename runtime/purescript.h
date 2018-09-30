#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

/* #define PURS_DEBUG_FINALIZATION */
/* #define PURS_DEBUG_SCOPES */

#define purs_malloc(sz) GC_MALLOC(sz)
#define purs_new(exp) GC_NEW(exp)

#ifdef ANY
#error macro 'ANY' already defined
#endif

#ifdef APP
#error macro 'APP' already defined
#endif

#define ANY purs_any_t
#define APP purs_any_app

#include <gc.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define purs_any_int_t int32_t
#define purs_any_num_t double
typedef struct purs_any purs_any_t;
typedef struct purs_any_cont purs_any_cont_t;
typedef union purs_any_value purs_any_value_t;
typedef const ANY * (purs_any_fun_t)(const void * ctx, const ANY *);

typedef enum {
	PURS_ANY_TAG_UNKNOWN = 0,
	PURS_ANY_TAG_INT = 1,
	PURS_ANY_TAG_NUM = 2,
	PURS_ANY_TAG_CONT = 3,
	PURS_ANY_TAG_THUNK = 4,
} purs_any_tag_t;

struct purs_any_cont {
	purs_any_fun_t * fn;
	const void * ctx;
};

union purs_any_value {
	purs_any_int_t i;
	purs_any_num_t n;
	purs_any_cont_t cont;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

const ANY * purs_any_app(const ANY * f, const ANY * v) {
	if (f->tag == PURS_ANY_TAG_CONT) {
		return f->value.cont.fn(f->value.cont.ctx, v);
	}
	return NULL;
}

const ANY * purs_any_int_new(const purs_any_int_t);
const ANY * purs_any_num_new(const purs_any_num_t);
const ANY * purs_any_cont_new(const void * ctx, purs_any_fun_t *);

#define PURS_ANY_THUNK_DEF(NAME, INIT)\
	static const ANY * NAME ## __thunk_fn__ (const void * __unused__1, const ANY * __unused__2) { \
		static const purs_any_t * NAME ## __thunk_val__ = NULL;\
		if (NAME ## __thunk_val__ == NULL) {\
			NAME ## __thunk_val__ = INIT;\
		}\
		return NAME ## __thunk_val__;\
	}\
	static const purs_any_t NAME ## __thunk__ = {\
		.tag = PURS_ANY_TAG_THUNK,\
		.value = {\
			.cont = {\
				.fn = NAME ## __thunk_fn__,\
				.ctx = NULL\
			}\
		}\
	};\
	const purs_any_t * NAME = & NAME ## __thunk__;\

#define PURS_SCOPE_T(NAME, DECLS)\
	typedef struct NAME {\
		struct DECLS;\
	} NAME

#define PURS_FFI_FUNC_0(NAME, BODY)\
	const ANY * NAME (void * super) BODY

#define PURS_FFI_FUNC_1(NAME, A1, BODY)\
	const ANY * NAME (const void * super, const ANY * A1) BODY

#define PURS_FFI_FUNC_2(NAME, A1, A2, BODY)\
	PURS_SCOPE_T(NAME##__ctx__1, { const ANY * A1; });\
	PURS_SCOPE_T(NAME##__ctx__2, { const ANY * A1; const ANY * A2; }); \
	const ANY * NAME##__2 (const void * super, const ANY * A2) {\
		NAME##__ctx__2 * ctx = purs_new(NAME##__ctx__2);\
		ctx->A1 = ((const NAME##__ctx__1*)super)->A1;\
		ctx->A2 = A2;\
		const ANY * A1 = ctx->A1;\
		BODY;\
	}\
	const ANY * NAME##__1 (const void * super, const ANY * A1) {\
		NAME##__ctx__1 * ctx = purs_new(NAME##__ctx__1);\
		ctx->A1 = A1;\
		return purs_any_cont_new(ctx, NAME##__2);\
	}\
	const ANY NAME##__1_ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = { .fn = NAME##__1, .ctx = NULL } }\
	};\
	const ANY * NAME = & NAME##__1_

#define PURS_FFI_FUNC_3(NAME, A1, A2, A3, BODY)\
	PURS_SCOPE_T(NAME##__ctx__1, { const ANY * A1; });\
	PURS_SCOPE_T(NAME##__ctx__2, { const ANY * A1; const ANY * A2; }); \
	PURS_SCOPE_T(NAME##__ctx__3, { const ANY * A1; const ANY * A2; const ANY * A3; }); \
	const ANY * NAME##__3 (const void * super, const ANY * A3) {\
		NAME##__ctx__3 * ctx = purs_new(NAME##__ctx__3);\
		ctx->A1 = ((const NAME##__ctx__2*)super)->A1;\
		ctx->A2 = ((const NAME##__ctx__2*)super)->A2;\
		ctx->A3 = A3;\
		const ANY * A1 = ctx->A1;\
		const ANY * A2 = ctx->A2;\
		BODY;\
	}\
	const ANY * NAME##__2 (const void * super, const ANY * A2) {\
		NAME##__ctx__2 * ctx = purs_new(NAME##__ctx__2);\
		ctx->A1 = ((const NAME##__ctx__1*)super)->A1;\
		ctx->A2 = A2;\
		return purs_any_cont_new(ctx, NAME##__3);\
	}\
	const ANY * NAME##__1 (const void * super, const ANY * A1) {\
		NAME##__ctx__1 * ctx = purs_new(NAME##__ctx__1);\
		ctx->A1 = A1;\
		return purs_any_cont_new(ctx, NAME##__2);\
	}\
	const ANY NAME##__1_ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = { .fn = NAME##__1, .ctx = NULL } }\
	};\
	const ANY * NAME = & NAME##__1_

#endif // PURESCRIPT_RUNTIME_H
