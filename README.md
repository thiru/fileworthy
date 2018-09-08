# Fileworthy

A simple website to manage your notes and files across all your devices.

## Installation

TODO

## Usage

TODO

## Develop

To start developing you'll probably want to run:

```shell
$ scripts/fileworthy.sh repl -e dev
```

This will:

* Start the website in development mode
* Launch a Rebel Readline REPL
* Start an nREPL server
* Start Figwheel Main

Then go to **http://localhost:8023**.

The above will leave you in a ClojureScript REPL. To access the Clojure
environment exit the ClojureScript REPL with CTRL-D. You can go back to it
with `(cljs)`.

Or, if you'd rather have a dedicated REPL for working with your Clojure
codebase while also having the existing Rebel Readline ClojureScript REPL, run
the following to connect to the existing nREPL server:

```shell
$ lein repl :connect 
```
