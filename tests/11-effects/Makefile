default: main
.PHONY: default

PUREC_DIR := ../..
include $(PUREC_DIR)/mk/target.mk

main: .spago

main_CFLAGS = -g
main_LD_FLAGS = -lm

$(eval $(call purs_mk_target,main,Main,src))
