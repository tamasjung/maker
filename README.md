
[](doc/Cima_da_Conegliano_God_the_Father.jpg)

# maker

## The basic idea

It is always better to separate the 'what' from the 'how', it is better to put
dependency as a value rather than as the result of another function call.

This is an experiment to see what happens if we structuring our system as goals
(values/things/nouns) and dependencies between them.

## What is the problem again?

[Watch this](https://youtu.be/Z6oVuYmRgkk?t=9m54s). A bit
different domain and a different solution but he is speaking about the same
issue in the next 3 minutes.

## maker is

  * still in alpha!
    * pls help to improve and give feedback
  * influenced by dependency injection frameworks, goal oriented programming,
  declarative programming, build tools, data flow programming,
  value-level programming, transducers

## Features

* dependency tracking by looking for functions based on the names
of parameters 
* works across namespaces,
* asynchronous goals, parallel execution,
* implicit dependencies,
* support for giving spec for goals efficiently

## Usage, learn
```clj
[maker "2.0.1"]
```
To learn [check out](test/maker/core_test.clj) and play with the didactic tests.

See how to add spec to your goals: [spec_test.clj](test/maker/spec_test.clj)
## License

Copyright © 2015-2017 Tamás Jung

Distributed under the MIT License.
