default: main
.PHONY: default

PUREC_DIR := ../..
export PATH := $(PUREC_DIR)/node_modules/.bin:$(PATH)
include $(PUREC_DIR)/mk/target.mk

$(eval $(call purs_mk_target,main,Example2,src))
