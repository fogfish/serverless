# serverless

Serverless support for Erlang applications.


## Inspiration

Run code without provisioning or managing servers is a modern way to deliver applications. This library enables Erlang runtime at AWS Lambda service using [child processes in Node.js](https://aws.amazon.com/blogs/compute/running-executables-in-aws-lambda/) techniques.

The library uses [escript](http://erlang.org/doc/man/escript.html) executables to package, debug and deploy lambda functions.  

## Getting started

The latest version of the library is available at its `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.

Add the library as dependency to rebar.config

```
{deps, [
   {serverless, ".*",
      {git, "https://github.com/fogfish/serverless", {branch, master}}
   }
]}.
```

### Workflow

The library defines a workflow of distribution Erlang application from sources to the cloud. The workflow builds a distribution package of Erlang application using rebar3 with help of Makefile orchestration. The file [serverless.mk](serverless.mk) implements the workflow

**New function**

The easiest way to start with Erlang serverless function is a template provided by [serverless.mk](serverless.mk). 

Download the workflow orchestration file

```
curl -O -L https://raw.githubusercontent.com/fogfish/serverless/master/serverless.mk
``` 

Create a Makefile

```Makefile
APP      = name-of-my-function

ENV     ?= dev
ROLE    ?= arn:aws:iam::000000000000:role/my-function-role
TIMEOUT ?= 30
MEMORY  ?= 256

include serverless.mk
```

Generate empty function

```
make function
```

The command downloads a [docker images](https://github.com/fogfish/erlang-in-docker) with serverless runtime, creates a source code of identity function, empty test suites and rebar config files. 

You project is ready for development.


**Compile and Test**

The easiest way to compile and test the application is the default Makefile target. You can also invoke compile and test targets sequentially.

```
make
```

**Make release**

Use **dist** target to assemble an escript executable containing the project's and its dependencies' BEAM files. This command also build a zip package containing Erlang runtime and executables of your function 

```
make dist
```

As the result, it produces `name-of-my-function-{version}.zip` package and `_build/default/bin/name-of-my-function` executables.


**Test release**

You can test executables before, they are deployed to the cloud. Executable accepts and produces JSONs from/to standard io. 

```
echo "{}" | _build/default/bin/name-of-my-function
```

**Deploy function**

The workflow implements a simple commands to deploy or patch AWS a function.

```
make config
```

Congratulations, your function is ready! 


## How To Contribute

The library is Apache 2.0 licensed and accepts contributions via GitHub pull requests:

* Fork the repository on GitHub
* Read build instructions
* Make a pull request

The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 20.0 or later and essential build tools.

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
