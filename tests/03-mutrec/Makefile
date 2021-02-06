default: main
.PHONY: default

export USE_GC := 1

PUREC_DIR := ../..
include $(PUREC_DIR)/mk/target.mk

$(eval $(call purs_mk_target,main,Main,src))
