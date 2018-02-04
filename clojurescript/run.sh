#!/bin/sh

PREFIX=/Users/kashyap/Documents/dev/clojure-bin/LIBS
JARS=$PREFIX/data.json-0.2.6.jar:$PREFIX/clj-http-2.2.0.jar:$PREFIX/clj-tuple-0.2.2.jar:$PREFIX/clojure-1.9.0.jar:$PREFIX/commons-codec-1.10.jar:$PREFIX/commons-io-2.4.jar:$PREFIX/commons-logging-1.2.jar:$PREFIX/core.specs.alpha-0.1.24.jar:$PREFIX/data.generators-0.1.2.jar:$PREFIX/httpclient-4.5.1.jar:$PREFIX/httpcore-4.4.4.jar:$PREFIX/httpmime-4.5.1.jar:$PREFIX/jsr166y-1.7.0.jar:$PREFIX/potemkin-0.4.3.jar:$PREFIX/riddley-0.1.12.jar:$PREFIX/slingshot-0.12.2.jar:$PREFIX/spec.alpha-0.1.143.jar:$PREFIX/test.check-0.9.0.jar:$PREFIX/test.generative-0.5.2.jar:$PREFIX/tools.namespace-0.2.10.jar

java -cp cljs.jar:src:$JARS clojure.main $*
