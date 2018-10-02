#ifndef Example2_H
#define Example2_H

#include <purescript.h>

PURS_FFI_FUNC_1(Example2_runGC, _, {
	GC_gcollect();
	return NULL;
});

PURS_FFI_FUNC_2(Example2_usleep, x, _, {
	usleep(purs_any_get_int(x));
	return NULL;
});

PURS_FFI_FUNC_1(Example2_unsafeCoerce, x, {
	return x;
});

PURS_FFI_FUNC_2(Example2_consoleLog, s, _, {
	printf("%s\n", purs_any_get_string(s));
	return NULL;
});

#endif // Example2_H
