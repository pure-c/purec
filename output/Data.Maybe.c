#include "Data.Maybe.h"

purs_any_t Data_Maybe_Nothing =
	{
		.tag = CONS,
		.value = {
			.cons = {
				.tag = Data_Maybe_Nothing__Tag
			}
		}
	};

purs_any_t * Data_Maybe_Just (purs_any_t * value0) {
	purs_any_t ** values = GC_MALLOC(sizeof (purs_any_t*[1]));
	values[0] = value0;
	return purs_any_set_cons(
		GC_NEW(purs_any_t),
		(purs_cons_t) {
			.tag = Data_Maybe_Just__Tag,
			.len = 1,
			.values = values
		}
	);
}

Data_Show_Show * Data_Maybe_showMaybe (Data_Show_Show * dictShow) {
	Data_Show_Show * value0 = GC_NEW(Data_Show_Show);
	value0->show =
		PURS_ANY_BLOCK((purs_any_t * x) {
				purs_cons_t * value1 = purs_any_get_cons(x);

				if (value1->tag == Data_Maybe_Just__Tag) {
					return purs_any_app(Data_Show_show(dictShow), value1->values[0]);
				}

				if (value1->tag == Data_Maybe_Nothing__Tag) {
					char * x = GC_MALLOC(100);
					strcpy(x, "Nothing");
					purs_any_t * y = purs_any_set_c_string(
						GC_NEW(purs_any_t),
						x
					);
					return y;
				}

				return (purs_any_t *) NULL; /* failed pattern match */
		});
	return value0;
}
