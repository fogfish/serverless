APP = helloworld

ENV      ?= dev
SERVICE  ?= ${ENV}-stack
LOGS_TTL ?= 5
BUCKET   ?= s3://packages/${SERVICE}
EVENT    ?= test/event.json

include ../../serverless.mk
