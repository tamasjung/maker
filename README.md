
[](doc/Cima_da_Conegliano_God_the_Father.jpg)

# maker

A Clojure library to explore inversion of control technique in a functional way. 

## The basic idea

Instead of 

```clojure
(defn a []
   ,,)

(defn b []
  ,,(a),,)

(defn c []
  ,,(b),,)

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
tomorrow then `b` needs too. Changes in `a`'s parameter/dependency list impact
`b`'s dependencies. This escalation is really bad. Even worse when you realize
that you want to resolve `a` with an asynchronous call. Not so when you defer
this dependency resolving up to c*.

We are not programming this way because c* does not scale well in terms of code
complexity, the big `let` is an anti-pattern. But wait, the big let could be
written by a macro, just by following the name of the parameters, like in c2*!
The transformation from a-b-c to a*-b*-c2* is not sufficient/possible all the
time but maker has some tricks which can extend the number of use cases.

With maker you will have more independent and purer functions, less global
variables in your system. As a 'side effect', with some helper functions
you can replace your current much hated component/reloaded framework or library
(search for 'reloaded' in `core-test`).

## It is

  * still experimental! do not be fooled by the major version number
    * help to improve, eat it, give feedback, fork it
  * influenced by
    * dependency injection frameworks
    * goal oriented programming
    * declarative programming
    * build tools
    * data flow programming
    * value-level programming
    * transducers
  * small
    * tough not minimal/optimal, yet

## Features

* dependency tracking by looking for functions based on the names of parameters
* works across namespaces
* asynchronous goals, parallel execution
* dynamic parameter/dependency list

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

## License

Copyright © 2015-2016 Tamás Jung

Distributed under the MIT License.
