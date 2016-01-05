# Copyright 2014 Jean Parpaillon, all rights reserved
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
PROJECT = averell
VERSION = 1.2.1

DESTDIR ?=

define localdep =
$(shell erl -noshell -eval "case application:ensure_all_started($1) of {ok, _} -> halt(0); _ -> halt(1) end." && echo ok || true)
endef

ALL_DEPS = cowboy getopt
$(foreach dep,$(ALL_DEPS),$(if $(call localdep,$(dep)),$(eval LOCAL_DEPS+=$(dep)),$(eval DEPS+=$(dep))))

dep_cowboy_commit = 2.0.0-pre.1
dep_getopt = git https://github.com/jcomellas/getopt.git v0.8.2

ESCRIPT_EMU_ARGS = -smp auto -pa . -noshell -noinput -sasl errlog_type error -escript main averell

DB2MAN = /usr/share/sgml/docbook/stylesheet/xsl/docbook-xsl/manpages/docbook.xsl
XP     = xsltproc -''-nonet -''-param man.charmap.use.subset "0"
MANS   = $(PROJECT).1

VSN = $(shell $(CURDIR)/version.sh $(VERSION))
ARCHIVE = $(PROJECT)-$(VSN).tar.xz

prefix ?= /usr/local
bindir ?= $(prefix)/bin

subst = sed -e 's|@VSN[@]|$(VSN)|g'

include erlang.mk

all:: escript

doc: man

test-build:: escript

escript::

man: $(MANS)

%.1: %.1.xml
	$(XP) $(DB2MAN) $<

ebin/$(PROJECT).app:: src/$(PROJECT).app.src

src/$(PROJECT).app.src: src/$(PROJECT).app.src.in
	$(gen_verbose) $(subst) $< > $@

dist: $(PROJECT)-$(VSN).tar.xz

debian-dist: $(PROJECT)_$(VSN).orig.tar.xz

$(PROJECT)_$(VSN).orig.tar.xz: $(PROJECT)-$(VSN).tar.xz
	ln -s $< $@

$(PROJECT)-$(VSN).tar.xz:
	-rm -f src/$(PROJECT).app.src
	@$(MAKE) --no-print-directory src/$(PROJECT).app.src
	$(gen_verbose) git archive --prefix=$(PROJECT)-$(VSN)/ HEAD . | \
	  tar xf - && \
	  cp src/$(PROJECT).app.src $(PROJECT)-$(VSN)/src && \
	  tar cf - $(PROJECT)-$(VSN) | xz > $@ && \
	  rm -rf $(PROJECT)-$(VSN)

install: all
	mkdir -p $(DESTDIR)$(bindir)
	install -m 755 $(PROJECT) $(DESTDIR)$(bindir)/$(PROJECT)

clean:: clean-deps clean-local

clean-deps:
	@for dep in $(wildcard deps/*) ; do \
		if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ] ; then \
			$(MAKE) -C $$dep clean ; \
		else \
			echo "include $(CURDIR)/erlang.mk" | ERLC_OPTS=+debug_info $(MAKE) -f - -C $$dep clean ; \
		fi ; \
	done

clean-local:
	- rm -f $(MANS)
	- rm -f $(PROJECT)

.PHONY: install
