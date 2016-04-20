
[](doc/Cima_da_Conegliano_God_the_Father.jpg)

#maker

A Clojure library to explore dependency injection technique in a functional way. 

* For data transformation applications it helps creating
    * more pure functions
    * more orthogonal functions
* It is 
  * a macro;
  * experimental!!!
  * is influenced by
    * old dependency injection frameworks
    * goal oriented programming
    * declerative programming
    * build tools
* It is not
  * a backtrack to OOP
  * is not a build automation specific tool but probably it would be easy to use for

##Features

* dependency tracking by looking for functions based on the parameter names
* works across namespaces
* handles 
  * iterations
  * conditionals

##Pros
* small
* building blocks are plain functions
* solves the parameter order issue
* save us from the boilerplate part of the code
* leverage Clojure's dependency notation of `ns` macro and `require` function

## Cons
* new tool - new mistakes
* needs some preparation for debugging

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
[tamasjung/maker "0.0.1"]
```
[See the tests](test/maker/core_test.clj)

## License

Copyright © 2015-2016 Tamás Jung

Distributed under the MIT License.
