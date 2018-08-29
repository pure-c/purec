# C backend for Purescript

## Status

### Things to come back to

* [x] Prefix purs_any_t tag names
* [ ] Allow specifying modifiers (i.e. `static` for `PURS_ANY_THUNK_DEF`)
* [ ] Do not expose `afmt`, instead create and export some `purs_afmt`
* [x] Rename 'purs_any_get_float' et. al. to '_number' for consistency
* [ ] Emit tabs instead of spaces
* [ ] Use less macross
* [ ] Handle `NULL` in more cases:
  * [ ] in AST for C.Accessor
* [ ] Deal with memory allocation problems (i.e. out of memory).
* [ ] Only emit exported declarations to header, and forward declare internal
  functions at top of implementation file.
* [ ] Use `static` modifier for functions that are not exported
* [x] Bake `True` and `False` into the runtime to avoid mallocing them in guards
  over and over.
* [ ] Do not use managed blocks (in `purs_any_t`) for lambdas that are generated
  and applied immediately with no arguments (ex. let bindings). We can avoid the
  malloc and GC registration for that block entirely.
