# serverless

Serverless support for Erlang applications, bootstrap your development on AWS Lambda. 


## Inspiration

Run code without provisioning or managing servers is a modern way to deliver applications. This library enables Erlang runtime at AWS Lambda service using [AWS Lambda Runtime Interface](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html). The Erlang runtime is deployed as [AWS Lambda Layer](https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html) to your AWS account.

The library uses [escript](http://erlang.org/doc/man/escript.html) executables to package, debug and deploy lambda functions.


## Key features

* Implements Erlang runtime for AWS Lambda service.
* Deploys Erlang\OTP as AWS Lambda Layer.
* Defines a lambda's life-cycle workflow.
* Provides command line tools to orchestrate local development and production deployments.


## Getting started

The latest version of the library is available at its `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.

Add the library as dependency to rebar.config

```erlang
{deps, [
   {serverless, ".*",
      {git, "https://github.com/fogfish/serverless", {branch, master}}
   }
]}.
```

The easiest way to start with Erlang serverless function development is a template provided by [serverless.mk](serverless.mk). Please look on [hello world](examples/helloworld) example as a play ground to get things up and running.


### Workflow

The library defines a life-cycle workflow to distribute Erlang application from sources to the cloud - [AWS Lambda service](https://aws.amazon.com/lambda/). The workflow builds a distribution package of Erlang application using rebar3 with help of Makefile orchestration. The file [serverless.mk](serverless.mk) implements the workflow:

1. [Configure AWS account](#configure-aws-account). 
2. [Create a new function](#create-new-function).
3. [Build function](#build-function)
4. [Run function locally](#run-function-locally)
5. [Package function](#package-function)
6. [Deploy function](#deploy-function)
7. [Clean up](#clean-up)


### Configure AWS account

Usage of [AWS Lambda Layers](https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html) simplifies an experience of lambda development and deployment. A custom Erlang runtime is provisioned with layers feature. The library uses docker image `fogfish/erlang-serverless` to distribute a runtime compatible with AWS Lambda service, images is managed by [erlang-in-docker](https://github.com/fogfish/erlang-in-docker).

You have to deploy a layer to you account before you'll be able to run any function. The following command builds a zip archive with Erlang runtime and deploys it your AWS account. Please notice, this operation has to be executed only once per AWS Account.

```
cd examples/helloworld

make layer
```


### Create a new function

The easiest way to start with Erlang serverless function development is a template provided by [serverless.mk](serverless.mk). Create a new folder for your function and download the workflow orchestration file. 

```bash
mkdir myfun && cd myfun

curl -O -L https://raw.githubusercontent.com/fogfish/serverless/master/serverless.mk
```

then, create a Makefile

```bash
cat > Makefile <<EOF
APP       = name_of_my_function

ENV      ?= dev
SERVICE  ?= \${ENV}-stack
LOGS_TTL ?= 5
BUCKET   ?= s3://packages/\${SERVICE}
EVENT    ?= test/event.json

include serverless.mk
EOF
```

The Makefile contains few configuration variables, they streamlines the deployment process

* `ENV` identity of deployment environment. This configuration variable supports managing a multi-environment serverless architecture in AWS using multi-stack approach (one stack per environment). A usual pipelines contains at least two different environments: development and production.
* `SERVICE` is used to prefix a function name during deployment processes.
* `LOGS_TTL` tile-to-live/retention period of lambda log streams.
* `BUCKET` identity of AWS S3 bucket to store lambda's artifacts.
* `EVENT` a default mock event used by local execution


Use build-in template to generate empty function

```bash
make function
```

The command downloads a [docker images](https://github.com/fogfish/erlang-in-docker) with serverless runtime, creates a source code of identity function, empty test suites, rebar config files and initializes deployment configurations. As the result, the following folder structure is created

```
+
+- cloud                           // cloud deployment configurations
|  +- dev                          // development environment
|  |  +- config.json               // lambda configurations (see aws lambda create-function)
|  |  +- source.json               // lambda source mappings (see aws lambda create-event-source-mapping)
| ...            
+- src                             // source code of lambda function
|  +- name-of-my-function.app.src
|  +- name-of-my-function.erl
+- test                            // tests of lambda function
|  +- event.json                   // specification of default event
|  |
| ...
+- rebar.config
```

You project is ready for development.


### Build function

Use default Makefile target to compile and test the application. Serverless library promotes a [Erlang Common Tests](http://erlang.org/doc/apps/common_test/introduction.html) for automated testing.

```bash
make
```

You can also invoke compile and test targets sequentially.

```bash
make compile
make test
```

The library provides a Lambda Runtime API mock `serverless:mock`, use to test your code.

```erlang
serverless:mock(
   helloworld,                 %% Function entry point
   #{},                        %% Mock input
   #{<<"helloworld">> => #{}}  %% Expect output
).
```


### Run function locally

Runs your Amazon Lambda function locally. It passes content of `test/event.json` as Amazon Lambda event object.

```bash
make run
```

Use `EVENT` variable to pass non default event

```bash
make run EVENT=test/kinesis.json
```


### Package function

Use **dist** target to assemble an escript executable containing the project's and its dependencies' BEAM files. This command also build a zip package containing executables of your function 

```bash
make dist
```

As the result, it produces `name-of-my-function-{version}.zip` package and `_build/default/bin/name-of-my-function` executables. Note, the version is deducted from latest git tag.

Please note you might need to explicitly package lambda to zip bundle if your are deploying it via AWS S3. Use **publish** target and `BUCKET` variable

```bash
make publish
```


### Deploy function

The workflow implements a target **deploy** to orchestrate deployment with help of [aws command line](https://aws.amazon.com/cli/).

To deploy a function, you need a configuration of target environment. This configuration contains reference to an execution role, lambda layer and other. The configuration format and parameters are 100% compatible with [aws lambda create-function](https://docs.aws.amazon.com/cli/latest/reference/lambda/create-function.html), please refers to official AWS documentation.

The serverless library manages environment configurations at `cloud` folder. A minimal configuration requires following arguments at `cloud/${ENV}/config.json`:

```json
{
  "FunctionName": "name_of_my_function",
  "Runtime": "provided",
  "Role": "arn:aws:iam::000000000000:role/my-role-function",
  "Handler": "index.handler",
  "Timeout": 10,
  "MemorySize": 256,
  "Publish": true,
  "Layers": [
    "arn:aws:lambda:eu-west-1:000000000000:layer:erlang-serverless:1"
  ]
}
```

Optionally, the lambda function can be associated with event sources(s) using [aws lambda create-event-source-mapping](https://docs.aws.amazon.com/cli/latest/reference/lambda/create-event-source-mapping.html). Sources are specified at `cloud/${ENV}/source.json`, trash this file if you need to skip an association with event sources. 

After the configuration is completed, deploy it

```bash
make deploy
```

Use the `ENV` variable to deploy to specific environment

```bash
make deploy ENV=live
```

Congratulations, your function is production ready!


### Clean up

A few targets are supported:

* **cloud-free** remove lambda deployment(s) at specified environment.
* **clean** build artifacts.
* **distclean** clean up everything except source code.


## How To Contribute

The library is Apache 2.0 licensed and accepts contributions via GitHub pull requests:

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

The development requires [Erlang/OTP](http://www.erlang.org/downloads) version 20.0 or later and essential build tools.

### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>
>

### bugs

If you experience any issues with the library, please let us know via [GitHub issues](https://github.com/fogfish/serverless/issue). We appreciate detailed and accurate reports that help us to identity and replicate the issue. 

* **Specify** the configuration of your environment. Include which operating system you use and the versions of runtime environments. 

* **Attach** logs, screenshots and exceptions, in possible.

* **Reveal** the steps you took to reproduce the problem, include code snippet or links to your project.


## License

Copyright 2018 Dmitry Kolesnikov

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
