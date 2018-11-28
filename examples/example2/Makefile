default: main
.PHONY: default

PUREC_DIR := ../..
include $(PUREC_DIR)/mk/target.mk

.psc-package:
	psc-package install
main: .psc-package

$(eval $(call purs_mk_target,main,Example2,src))
