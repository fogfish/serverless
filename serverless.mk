##
## Copyright (C) 2018 Dmitry Kolesnikov
##
## This Makefile may be modified and distributed under the terms
## of the MIT license.  See the LICENSE file for details.
## https://github.com/fogfish/serverless
##
## @doc
##   This makefile is the wrapper of rebar to build serverless applications
##
## @version 0.6.0
.PHONY: all compile test dist distclean dist-up dist-rm

APP    := $(strip $(APP))
VSN    ?= $(shell test -z "`git status --porcelain 2> /dev/null`" && git describe --tags --long 2> /dev/null | sed -e 's/-g[0-9a-f]*//' | sed -e 's/-0//' || echo "`git describe --abbrev=0 --tags 2> /dev/null`-a")
TEST   ?= tests
REBAR  ?= 3.9.1
DOCKER  = fogfish/erlang-serverless:20.3

## erlang runtime configration flags
ROOT   = $(shell pwd)
ADDR   = localhost.localdomain
EFLAGS = \
	-name ${APP}@${ADDR} \
	-setcookie ${COOKIE} \
	-pa ${ROOT}/_build/default/lib/*/ebin \
	-pa ${ROOT}/_build/default/lib/*/priv \
	-pa ${ROOT}/rel \
	-kernel inet_dist_listen_min 32100 \
	-kernel inet_dist_listen_max 32199 \
	+P 1000000 \
	+K true +A 160 -sbt ts

#####################################################################
##
## build
##
#####################################################################
all: rebar3 test

compile: rebar3
	@./rebar3 compile

run: _build/default/bin/${APP}
	@test -z ${JSON} \
		&& $^ -f ${EVENT} \
		|| { T=`mktemp /tmp/lambda.XXXXXXX` ; trap "{ rm -f $$T; }" EXIT ;\
			jq -n --arg json "`cat ${JSON}`" -f ${EVENT} > $$T | $^ -f $$T; }

shell:
	@erl ${EFLAGS}

##
## execute common test and terminate node
test:
	@./rebar3 ct --config test/${TEST}.config --cover --verbose
	@./rebar3 cover

##
## clean 
clean:
	-@./rebar3 clean
	-@rm -rf _build/builder
	-@rm -rf _build/layer
	-@rm -rf _build/*.zip
	-@rm -rf log
	-@rm -rf _build/default/bin

##
##
dist: _build/default/bin/${APP} _build/default/bin/bootstrap

_build/default/bin/bootstrap:
	@printf "#!/bin/sh\nexport HOME=/opt\n/opt/serverless/bin/escript ${APP}\n" > $@ ;\
	chmod ugo+x $@

_build/default/bin/${APP}: src/*.erl src/*.app.src
	@./rebar3 escriptize

##
##
distclean: clean
	-@rm -Rf _build
	-@rm rebar3
	-@rm -Rf cloud/node_modules


function:
	@I=`docker create ${DOCKER}` ;\
	docker cp $$I:/function/cloud . ;\
	docker cp $$I:/function/src . ;\
	docker cp $$I:/function/test . ;\
	docker cp $$I:/function/rebar.config . ;\
	docker rm -f $$I ;\
	sed -i '' -e "s/APP/${APP}/" cloud/* ;\
	sed -i '' -e "s/APP/${APP}/" src/* ;\
	sed -i '' -e "s/APP/${APP}/" test/* ;\
	sed -i '' -e "s/APP/${APP}/g" rebar.config ;\
	mv src/app.app.src src/${APP}.app.src ;\
	mv src/app.erl src/${APP}.erl ;\
	mv test/app_SUITE.erl test/${APP}_SUITE.erl ;\
	mkdir -p test ;\
	echo "{}" > ${EVENT}


#####################################################################
##
## deploy
##
#####################################################################

dist-up: dist cloud/node_modules
	cd cloud && cdk deploy

dist-rm: cloud/node_modules
	cd cloud && cdk destroy -f

cloud/node_modules:
	cd cloud && npm install

layer: _build/erlang-serverless.zip
	@aws lambda publish-layer-version \
		--layer-name erlang-serverless \
		--description "${DOCKER}" \
		--zip-file fileb://./$^

_build/erlang-serverless.zip:
	@mkdir -p _build ;\
	echo "FROM ${DOCKER}\nRUN cd /opt && zip erlang-serverless.zip -r * > /dev/null" > _build/layer ;\
	docker build --force-rm=true --tag=build/erlang-serverless:latest - < _build/layer ;\
	I=`docker create build/erlang-serverless:latest` ;\
	docker cp $$I:/opt/erlang-serverless.zip $@ ;\
	docker rm -f $$I ;\
	docker rmi build/erlang-serverless:latest ;\
	test -f $@ && echo "==> $@"


#####################################################################
##
## dependencies
##
#####################################################################
rebar3:
	@echo "==> install rebar (${REBAR})" ;\
	curl -L -O -s https://github.com/erlang/rebar3/releases/download/${REBAR}/rebar3 ;\
	chmod +x $@
