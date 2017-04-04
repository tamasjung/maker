
[](doc/Cima_da_Conegliano_God_the_Father.jpg)

# maker

A Clojure library to explore dependency injection technique in a functional way. 

* It is 
  * a macro;
  * experimental!!!
  * influenced by
    * old dependency injection frameworks
    * goal oriented programming
    * declerative programming
    * build tools
* It is not
  * a backtrack to OOP
  * a build automation specific tool but probably it would be easy to use for

## Features

* dependency tracking by looking for functions based on the parameter names
* works across namespaces
* handles 
  * iterations
  * conditionals

## Possible future directions
* other execution plans (the current one is depth-first serial)
  * for parallel computing 
  * for asynchronous goals
* adoption in ClojureScript
* configurable mapping from parameter to maker function
* pluggable extensions
* special type checker

## Usage
```clj
[maker "0.1.2"]
```
[See the tests](test/maker/core_test.clj)

## License

Copyright © 2015-2016 Tamás Jung

Distributed under the MIT License.
