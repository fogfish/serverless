APP = helloworld
ORG = 

ENV      ?= dev
STACK    ?= ${ENV}-stack
LOGS_TTL ?= 5
CODE     ?= s3://packages/${STACK}
EVENT    ?= test/event.json

include ../../serverless.mk
