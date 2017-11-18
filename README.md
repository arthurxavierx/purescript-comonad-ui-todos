# purescript-comonad-ui-todos

Simple task management application inspired by [TodoMVC](todomvc.com); written for my bachelor thesis based on Phil Freeman's [Comonads as spaces](http://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html).

This simple application demonstrates three types of UIs modelled with three different comonads, namely:

- the `Store` comonad: models a general architecture where every component fully exposes its state for read and write operations;
- the `Moore i` comonad: isomorphic to `Cofree ((->) i)` or `Traced [i]`, this comonad models the Elm architecture where user component inputs are given by a type `i`;
- The `Cofree f` comonad: models an object-oriented architecture similar to that of [Halogen](https://github.com/slamdata/purescript-halogen).

We have implemented local storage in the browser for saving tasks for all UI examples, except for the `Moore` one where it is still unclear how we could access the modified global model in order to store it.
