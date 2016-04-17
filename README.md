
![](doc/Cima_da_Conegliano_God_the_Father.jpg)

#maker
* It is 
  * a functional dependency injection tool;
  * a functional programming style which helps creating
    * more pure functions
    * more orthogonal functions
  * software architecture which
    * separates logic from execution manner 
  * a macro;
  * experimental!!!
  * written in Clojure for Clojure apps
  * influenced by
    * old dependency injection frameworks
    * goal oriented programming
    * declerative programming
  * NOT a backtrack to OOP
  * NOT primarly for managing components' live cycle but can be used for that too
  * NOT a build automation specific tool but probably it would be easy to ...
* Features
  * dependency tracking by looking for functions based on parameter names
  * works across namespaces
  * handles 
    * iterations
    * conditionals
* Pros
  * small
  * based on plain functions
  * solves the parameter order issue
  * save us from the boilerplate part of the code
  * leverage Clojure's dependency notation of `ns` macro and `require` function
* Cons
  * new style - new mistakes
  * harder to debug 
* Future plans, ideas
  * other execution plans (the current one is depth-first serial)
    * for parallel computing 
    * for asynchronous goals
  * adoption in ClojureScript
  * configurable mapping from parameter to maker function
  * pluggable extensions
  * type checking, specifying the return type would be enough

## Usage

FIXME

## License

Copyright © 2015-2016 Tamás Jung

Distributed under the MIT License.
