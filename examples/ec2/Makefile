APP = ec2
ORG = 

ENV     ?= dev
STACK   ?= ${ENV}-stack
ROLE    ?= arn:aws:iam::000000000000:role/${STACK}-role-function
TIMEOUT ?= 30
MEMORY  ?= 128
CODE    ?= s3://packages/${STACK}
LAYER   ?= arn:aws:lambda:eu-west-1:000000000000:layer:erlang-serverless:1

include ../../serverless.mk
