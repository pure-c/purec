default: main
.PHONY: default

export PATH := node_modules/.bin:$(PATH)
PUREC_DIR := ../..
include $(PUREC_DIR)/mk/target.mk

SHELL := /bin/bash
srcs := .
deps := $(shell\
	find ../bower_components/purescript-{control,effect,prelude,console}/src/\
	    -type f\
	    -name '*.purs')
$(eval $(call purs_mk_target,main,Main,$(srcs),$(deps)))
