default: main
.PHONY: default

CFLAGS = -g3 -O0

PUREC_DIR := ../..
include $(PUREC_DIR)/mk/target.mk

main: .spago

$(eval $(call purs_mk_target,main,Main,src))
