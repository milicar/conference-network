# conference-network

## What does it do?

This is a small application that tries to visualise the communication
about an event between event participants, based on their tweets. A graph
is created for this communication, which can then be saved under user's
account.

This application also tries to predict which of the participants will join
the same conference next year. For this, a decision tree classifier is
implemented. Input features consist of graph metrics, such as centralities,
belonging to a clique or a component, and metrics based on tweets (so far,
only the number of tweets per participant). Decision tree can take early
stopping parameters - maximum tree depth and minimum node size, or grow fully, and
then be pruned. The optimal model candidates are found by cross validation,
and then compared on test dataset.

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

This application was developed for *'Software engineering tools and methods'*
and *'Intelligent Information Systems'* courses on Master studies of the
Software Engineering and Computer Science study program of Faculty of
 Organisational Sciences, University of Belgrade.
