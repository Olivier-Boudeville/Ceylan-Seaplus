![Erlang CI](https://github.com/Olivier-Boudeville/Ceylan-Seaplus/workflows/Erlang%20CI/badge.svg)

# Ceylan-Seaplus

![](/doc/seaplus-title.png)


This repository corresponds to the part of the [Ceylan project](https://github.com/Olivier-Boudeville/Ceylan) gathering facilities introduced in order to **better integrate C/C++ code to Erlang**.

This binding relies on a **port** (not a NIF) based on a **C-Node** and the standard **ei** (formerly: Erl_Interface), so that the integrated C/C++ code cannot jeopardise the operation of the Erlang VM (hence at the cost of a few message exchanges, yet with a low overhead, marshalling/demarshalling having to happen anyway).

**A parse transform is used in order to ease the definition of both sides** (Erlang and C) of the corresponding driver (generating the former integrally, and the latter partially; moreover a C library gathering higher-level primitives is provided in order to facilitate the development of the rest of the C side).

A complete, functional example of a service to integrate is provided (see the [foobar test C project](https://github.com/Olivier-Boudeville/Ceylan-Seaplus/tree/master/tests/c-test/foobar)).

Please refer to the [Seaplus official documentation](http://seaplus.esperide.org), otherwise to its [mirror](http://olivier-boudeville.github.io/Ceylan-Seaplus/).

The 'master' branch corresponds to the current stable version of this layer.

Ceylan-Seaplus relies on [Ceylan-Myriad](https://github.com/Olivier-Boudeville/Ceylan-Myriad).
