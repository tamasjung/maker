
[](doc/Cima_da_Conegliano_God_the_Father.jpg)

# maker



## The basic idea

It is always better to separate the 'what' from the 'how'. Let's define
transformations of things if it is possible. Then we can decide later how to run
these transformations. Most of the applications we write can be modelled
naturally this way.

Instead of 

```clojure
(defn a [,,]
   ,,)

(defn b [,,]
  ,,(a ,,),,)

(defn c [,,]
  ,,(b,,),,)

```
we should try this
```clojure

(defn a* []
  ,,)

(defn b* [a]
  ,,a ,,)

(defn c* []
  (let [a (a*)
        b (b* a)]
    ,,b,,))
    
    
(defn c2* []
  ,,(make b),,)
```
In the former `b` depends on function `a` function. If `a` needs a new parameter
tomorrow then `b` needs too. Even worse when you realize that you want to make
the computation in `a`asynchronous. Not so when you defer this dependency
resolving up to c*.

We are not programming this way because c* does not scale well in terms of code
complexity, the big `let` is an anti-pattern. But wait, the big `let` could be
generated by a macro, just by following the name of the parameters, like in c2*!
The transformation from a-b-c to a*-b*-c2* is not sufficient or possible all the
time but maker has some tricks which can increase the number of use cases.

Using this library you will have more independent modules and higher number of
pure functions, less global variables in your system. With some helper functions
you can make your own simple reloaded/component support (for more information
search for 'reloaded' in `core-test`).

## It is

  * still experimental! do not be fooled by the major version number
    * pls help to improve, give feedback
  * influenced by
    * dependency injection frameworks
    * goal oriented programming
    * declarative programming
    * build tools
    * data flow programming
    * value-level programming
    * transducers
  * small
    * tough not minimal/optimal yet
  * old ideas implemented on top of a shiny functional language
  * ideal for composing values

## Features

* dependency tracking by looking for functions based on the names of parameters
* works across namespaces
* asynchronous goals, parallel execution
* dynamic dependency list

## Future options
* adoption in ClojureScript
* optional static types or specs
  * another huge win: we need specify only the result, the params are already
  have done.

## Usage, learn
```clj
[maker "2.0.0-SNAPSHOT"] ;;after git clone, lein install
```
To learn [see](test/maker/core_test.clj) and play with the didactic tests.

## Known issues

* make with async goals is an error but not handled nicely.
* make<> probably does not work with AOT compilation.

## License

Copyright © 2015-2016 Tamás Jung

Distributed under the MIT License.
