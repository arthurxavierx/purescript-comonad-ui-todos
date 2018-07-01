# purescript-comonad-ui-todos

Simple task management application inspired by [TodoMVC](todomvc.com); written for the first part of my bachelor thesis [Comonads for User Interfaces](https://github.com/arthurxavierx/purescript-comonad-ui-todos/raw/master/ComonadsForUIs.pdf) based on Phil Freeman's [Comonads as spaces](http://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html).

This simple application demonstrates three types of UIs modelled with three different comonads, namely:

- the `Store` comonad: models a general architecture where every component fully exposes its state for read and write operations;
- the `Moore i` comonad: isomorphic to `Cofree ((->) i)` or `Traced [i]`, this comonad models the Elm architecture where user component inputs are given by a type `i`;
- the `Cofree f` comonad: models an object-oriented architecture similar to that of [Halogen](https://github.com/slamdata/purescript-halogen).

We have implemented local storage in the browser for saving tasks for all UI examples. In the `Moore` example, however, it is still unclear what could be the best method for applying effects when of a component action (as the component state is private). We have used an _ad hoc_ approach by replicating a `save` function when of every user action.

## Build instructions

In order to build this application one must have the [PureScript](http://www.purescript.org/) compiler (version 0.11.7) installed, as well as the [npm](https://www.npmjs.com/) and [bower](https://bower.io/) tools for package management.

After installing the prerequisites, simply run

```
make
```
