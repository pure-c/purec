#ifndef Example1_H
#define Example1_H

#include <purescript.h>

PURS_FFI_FUNC_2(Example1_putStr, s, _, {
	printf("%s", purs_any_get_string(s)->data);
	return NULL;
});

PURS_FFI_FUNC_2(Example1_exit, _code, _, {
	exit(*purs_any_get_int(_code));
	return NULL;
});

PURS_FFI_FUNC_2(Example1_putStrLn, s, _, {
	printf("%s\n", purs_any_get_string(s)->data);
	return NULL;
});

PURS_FFI_FUNC_UNCURRIED_2(Example1_getLineImpl, Just, Nothing, {
	size_t len;
	char * line = NULL;
	if (getline(&line, &len, stdin) != -1) {
		line[strlen(line) - 1] = 0; // remove trailing newline
		const purs_any_t * o = purs_any_app(Just, PURS_ANY_STRING_NEW(line));
		return o;
	} else {
		return Nothing;
	}
});

#endif // Example1_H
