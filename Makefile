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

DEPS = cowboy cowboy_cors getopt
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.1
dep_cowboy_cors = git http://github.com/jeanparpaillon/cowboy_cors v0.3.1
dep_getopt = git https://github.com/jcomellas/getopt.git v0.8.2

ESCRIPT_EMU_ARGS = -smp auto -pa . -noshell -noinput -sasl errlog_type error -escript main averell

DB2MAN = /usr/share/sgml/docbook/stylesheet/xsl/docbook-xsl/manpages/docbook.xsl
XP     = xsltproc -''-nonet -''-param man.charmap.use.subset "0"
MANS   = $(PROJECT).1

.DEFAULT_GOAL = escript

include erlang.mk

man: $(MANS)

%.1: %.1.xml
	$(XP) $(DB2MAN) $<

dist: deps
	$(MAKE) clean clean-deps
	vsn=$(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || true) && \
	  rm -rf averell-$${vsn} && \
	  git archive --prefix=averell-$${vsn}/ HEAD . | tar -xf - && \
	  tar -cf - \
	    --exclude-vcs \
	    --exclude='deps/cowboy/examples' --exclude='deps/cowboy/doc' --exclude='deps/cowboy/test' \
	    --exclude='deps/cowboy_cors/example' --exclude='deps/cowboy_cors/test' \
	    --exclude='deps/cowlib/test' \
	    --exclude='deps/getopt/doc' --exclude='deps/getopt/examples' --exclude='deps/getopt/test' \
	    --exclude='deps/ranch/examples' --exclude='deps/ranch/guide' --exclude='deps/ranch/manual' --exclude='deps/ranch/test' \
	    deps | tar -xf - -C averell-$${vsn} && \
	  tar -cf - averell-$${vsn} | xz > averell-$${vsn}.tar.xz && \
	  rm -rf averell-$${vsn}

clean-deps:
	@for dep in $(wildcard deps/*) ; do \
		if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ] ; then \
			$(MAKE) -C $$dep clean ; \
		else \
			echo "include $(CURDIR)/erlang.mk" | ERLC_OPTS=+debug_info $(MAKE) -f - -C $$dep clean ; \
		fi ; \
	done
