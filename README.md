# clr.tools.reader

A port [ clojure/tools.reader](https://github.com/clojure/tools.reader) library to ClojureCLR.

From the parent's README:

> A complete Clojure reader and an EDN-only reader, ...

See the parent repo for documentation.

# Releases

Latest stable release: 1.4.2

[clj](https://clojure.org/guides/getting_started) dependency information:
```clojure
io.github.clojure/clr.tools.reader {:git/tag "v1.4.2" :git/sha "c1186e3"}
```

Nuget reference:

> PM> Install-Package clojure.tools.reader -Version 1.4.2

[Leiningen](https://github.com/technomancy/leiningen) dependency information:
```clojure
   [org.clojure.clr/tools.reader "1.4.2"]
```


# Note to maintainers

If using lein-clr to work on this, specifically, to run tests, be aware that the structure of the tests in this project does not conform to the leiningen standard.  
There is a file common_tests.clj that is loaded by several other test files  
(Speaking to the notion that an include-file should be added for when load is not appropriate, as here).  
The leiningen test framework will load common_tests.clj and fail.  
You need to run each top-level file directly.  For example,

```
lein.bat clr test clojure.tools.metadata-test clojure.tools.reader-test clojure.tools.reader-edn-test
```

# Copyright and License #

Original Clojure(JVM) code: 

> Copyright © Nicola Mometto, Rich Hickey & contributors.

> Licensed under the EPL. (See the file epl.html.))
