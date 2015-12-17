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

DEPS = cowboy getopt
dep_cowboy_commit = 2.0.0-pre.1
dep_getopt = git https://github.com/jcomellas/getopt.git v0.8.2

ESCRIPT_EMU_ARGS = -smp auto -pa . -noshell -noinput -sasl errlog_type error -escript main averell

DB2MAN = /usr/share/sgml/docbook/stylesheet/xsl/docbook-xsl/manpages/docbook.xsl
XP     = xsltproc -''-nonet -''-param man.charmap.use.subset "0"
MANS   = $(PROJECT).1

include erlang.mk

all:: escript

doc: man

test-build:: escript

man: $(MANS)

%.1: %.1.xml
	$(XP) $(DB2MAN) $<

dist: deps
	$(MAKE) clean
	vsn=$(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || true) && \
	  rm -rf averell-$${vsn} && \
	  git archive --prefix=averell-$${vsn}/ HEAD . | tar -xf - && \
	  tar  --exclude='deps' -cf - averell-$${vsn} | xz > averell-$${vsn}.tar.xz && \
	  rm -rf averell-$${vsn}

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
