

# maker

## The basic idea

It is always better to separate the 'what' from the 'how'. It is better to have
dependency as a value provided by the 'system' (in transparent way) rather than as
a result of another function call.

This is an experiment to see what happens if we structure our system as goals
(values/things/nouns) and dependencies between them.

## What is the problem again?

[Watch this](https://youtu.be/Z6oVuYmRgkk?t=9m54s).
Different domain and a different solution, but he is speaking about the same
issue in the next 3 minutes.

## maker is

  * still in alpha!
    * pls help to improve and give feedback
  * influenced by dependency injection frameworks, goal oriented programming,
  declarative programming, build tools, data flow programming,
  value-level programming, transducers

## Features

* dependency tracking by looking for functions based on the names of the parameters
* efficient support for polymorphism (defmulticase, defcasegoal)
* parametric goal with defgoalfn  
* asynchronous goals, parallel execution,
* spec for goals,
* works across namespaces


## Usage, learn
```clj
[maker "3.2.2"]
```
To learn [check out](test/maker/core_test.cljc) and play with the didactic tests.

See how to add spec to your goals: [spec_test.clj](test/maker/spec_test.clj)

## Alternative

* From the amazing plumatic group: [plumbing](https://github.com/plumatic/plumbing)   

## License

Copyright © 2015-2024 Tamás Jung

Distributed under the MIT License.
