#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include "runtime/purescript.h"

#include ".purec-work/lib/Main.h"

int main(void) {
	return purs_any_get_int(purs_any_app(Main_main_$, purs_any_null));
}
