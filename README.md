# clr.tools.reader

A port [ clojure/tools.reader](https://github.com/clojure/tools.reader) library to ClojureCLR.

From the parent's README:

> A complete Clojure reader and an EDN-only reader, ...

# Releases

Nuget reference:

    PM> Install-Package clojure.tools.reader

Leiningen/Clojars reference:

   [org.clojure.clr/tools.reader "1.3.2.2"]


# Note to maintainers

If using lein-clr to work on this, specifically, to run tests, be aware that the structure of the tests in this project does not conform to the leiningen standard.  There is a file common_tests.clj that is loaded by several other test files  (Speaking to the notion that an include-file should be added for when load is not appropriate, as here).  The leiningen test framework will load common_tests.clj and fail.  You need to run each top-level file directly.  For example,

```
lein.bat clr test clojure.tools.metadata-test clojure.tools.reader-test clojure.tools.reader-edn-test
```

# Copyright and License #

Original Clojure(JVM) code: 

> Copyright © 2013-2018 Nicola Mometto, Rich Hickey & contributors.


Licensed under the EPL. (See the file epl.html.)
