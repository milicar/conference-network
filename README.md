# conference-network

## Prerequisites

You will need [Leiningen][1] 1.7.0 or above installed.
You will need MySql installed.

[1]: https://github.com/technomancy/leiningen

## Running

* Execute conference_network.sql script on your MySql server.
* Change database access configuration in db-config.txt.
* To start a web server for the application, run: lein ring server

## Libraries used

* Ring - web applications library
* Compojure - routing for Ring
* Hiccup - turning a Clojure data structure into a string of HTML
* Bouncer - validation DSL for Clojure & Clojurescript applications
* Lib-noir - a set of utilities and helpers for building ring apps.
* Twitter-API - library for accessing the Twitter API
* Ubergraph - general-purpose graph data structure for Clojure
* Midje - test framework
* Expect-call - library for mocking function calls
* Jungerer - clojure network/graph library wrapping JUNG.


## About

This application is developed for Software engineering tools and methods course on Master studies of the
Software Engineering and Computer Science study program of Faculty of Organisational Sciences, University of Belgrade.
