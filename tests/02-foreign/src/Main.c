#include "runtime/purescript.h"
#include "Main.h"

void buf_free(void* tag, void* data) {
	purs_free(tag);
	struct buf * buf = data;
	purs_free(buf->data);
	purs_free(buf);
}

PURS_FFI_FUNC_1(Main_newBuffer, _) {
	struct buf * buf = purs_new(struct buf);
	buf->data = NULL;
	buf->size = 0;
	return purs_any_foreign(purs_foreign_new(NULL, buf, buf_free));
}

PURS_FFI_FUNC_2(Main_bufferSize, x, _) {
	assert(x.tag == PURS_ANY_TAG_FOREIGN);
	struct buf * buf = (struct buf *) x.value.foreign->data;
	return purs_any_int(buf->size);
}

PURS_FFI_FUNC_3(Main_bufferGrow, n_, x, _) {
	assert(x.tag == PURS_ANY_TAG_FOREIGN);
	int n = purs_any_unsafe_get_int(n_);
	struct buf *buf = (struct buf *) x.value.foreign->data;
	char *data = purs_malloc(sizeof (char) * (buf->size + n));
	memcpy(data, buf->data, buf->size);
	purs_free(buf->data);
	buf->size += n;
	buf->data = data;
	return purs_any_int(buf->size);
}
