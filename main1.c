#include "Data.Maybe.h"

int main () {
	Data_Show_Show * x = Data_Maybe_showMaybe(NULL);
	purs_any_t * y = purs_any_app(x->show, &Data_Maybe_Nothing);
	printf("%s", y->value.c_string);
	return 0;
}
