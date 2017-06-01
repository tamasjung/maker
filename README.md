
[](doc/Cima_da_Conegliano_God_the_Father.jpg)

# maker

A Clojure library to explore inversion of control technique in a functional way. 

* It is 
  * still experimental! do not be fooled by the main version number
  * influenced by
    * dependency injection frameworks
    * goal oriented programming
    * declarative programming
    * build tools
    * data flow programming
    * value-level programming
    * transducers

## Features

* dependency tracking by looking for functions based on the names of parameters
* works across namespaces
* asynchronous goals
* dynamic parameter/dependency list


## Possible future directions
* adoption in ClojureScript

## Usage
```clj
[maker "1.0.0"]
```
[See the tests](test/maker/core_test.clj)

## License

Copyright © 2015-2016 Tamás Jung

Distributed under the MIT License.
