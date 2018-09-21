#ifndef Example1_H
#define Example1_H

#include <purescript.h>

PURS_FFI_FUNC_2(Example1_putStr, s, _, {
	printf("%s", purs_any_get_string(s)->data);
	return NULL;
})

PURS_FFI_FUNC_2(Example1_exit, _code, _, {
	exit(*purs_any_get_int(_code));
	return NULL;
})

PURS_FFI_FUNC_2(Example1_putStrLn, s, _, {
	printf("%s\n", purs_any_get_string(s)->data);
	return NULL;
})

PURS_FFI_FUNC_3(Example1_getLineImpl, Just, Nothing, _, {
	size_t len;
	char * line = NULL;
	if (getline(&line, &len, stdin) != -1) {
		line[strlen(line) - 1] = 0; // remove trailing newline
		return purs_any_app(Just, PURS_ANY_STRING_NEW(line));
	} else {
		return Nothing;
	}
})



#endif // Example1_H
