#ifndef Example1_FFI_H
#define Example1_FFI_H

#include "runtime/purescript.h"
#include <uv.h>

PURS_FFI_FUNC_1(Example1_runGC, _, {
	GC_gcollect();
	return NULL;
})

PURS_FFI_FUNC_2(Example1_usleep, x, _, {
	usleep(*purs_any_get_int(x));
	return NULL;
})

PURS_FFI_FUNC_1(Example1_unsafeCoerce, x, {
	return x;
})

PURS_FFI_FUNC_2(Example1_consoleLog, _s, _, {
	const void * s = purs_any_get_string(_s)->data;
	printf("%s\n", s);
	return NULL;
})

PURS_FFI_VALUE(
    Example1_uvRunDefault,
    PURS_ANY_INT(UV_RUN_DEFAULT)
);

PURS_FFI_FUNC_1(Example1_uvDefaultLoop, _, {
	return PURS_ANY_FOREIGN_NEW(NULL, uv_default_loop());
});

void _start_thread (void * arg) {
	struct GC_stack_base sb;
	GC_get_stack_base(&sb);
	GC_register_my_thread(&sb);
	purs_any_app((purs_any_t *) arg, NULL);
	GC_unregister_my_thread();
}

PURS_FFI_FUNC_2(Example1_uvThreadCreate, cb, _, {
	uv_thread_t * thread = malloc(sizeof (uv_thread_t));
	uv_thread_create(thread, _start_thread, cb);
	return PURS_ANY_FOREIGN_NEW(NULL, thread);
})

PURS_FFI_FUNC_2(Example1_uvThreadJoin, _thread, _, {
	uv_thread_t * thread = (uv_thread_t *) purs_any_get_foreign(_thread)->data;
	uv_thread_join(thread);
	return NULL;
})

PURS_FFI_FUNC_3(Example1_uvRun, _loop, _mode, _, {
	void * loop = purs_any_get_foreign(_loop)->data;
	uv_run_mode mode = (uv_run_mode) * purs_any_get_int(_mode);
	uv_run(loop,  mode);
	return NULL;
})

PURS_FFI_FUNC_2(Example1_plus, _a, _b, {
	return PURS_ANY_INT_NEW((*purs_any_get_int(_a)) + (*purs_any_get_int(_b)));
})

PURS_FFI_FUNC_2(Example1_xpure, _a, _, {
	return _a;
})

PURS_FFI_FUNC_3(Example1_xbind, _f, _k, _, {
	const purs_any_t * x = purs_any_app(_f, NULL);
	const purs_any_t * y = purs_any_app(purs_any_app(_k, x), NULL);
	return y;
})

#endif // Example1_FFI_H
