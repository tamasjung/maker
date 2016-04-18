
![](doc/Cima_da_Conegliano_God_the_Father.jpg)

#maker
* It is 
  * a functional dependency injection tool;
  * a functional programming style which helps creating
    * more pure functions
    * more orthogonal functions
  * software architecture which
    * separates logic, data transformation from execution manner 
  * a macro;
  * experimental!!!
  * written in Clojure for Clojure apps
  * influenced by
    * old dependency injection frameworks
    * goal oriented programming
    * declerative programming
    * build tools
  * not a backtrack to OOP
  * not primarly for managing components' live cycle but can be used for that too
  * not a build automation specific tool but probably it would be easy to ...
* Features
  * dependency tracking by looking for functions based on parameter names
  * works across namespaces
  * handles 
    * iterations
    * conditionals
* Pros
  * small
  * building blocks are plain functions
  * solves the parameter order issue
  * save us from the boilerplate part of the code
  * leverage Clojure's dependency notation of `ns` macro and `require` function
* Cons
  * new paradigm - new mistakes
  * needs preparing for debugging
* Possible future directions, ideas
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
