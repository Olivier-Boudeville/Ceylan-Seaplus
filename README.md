# Ceylan-Seaplus

![](/doc/seaplus-title.png)


This repository corresponds to the part of the [Ceylan project](https://github.com/Olivier-Boudeville/Ceylan) gathering facilities to better integrate C/C++ code to Erlang.

This binding relies on a port (not a NIF) based on a C-Node and the standard Erl_Interface, so that the integrated C/C++ code cannot jeopardise the operation of the Erlang VM (hence at the cost of a few message exchanges, marshalling/demarshalling having to happen anyway).

A parse transform is used in order to ease the definition of both sides (Erlang and C) of the corresponding driver, generating fully the former one, and partly the latter one).

A complete, functional example of a service to integrate is provided (see the [foobar test C project](https://github.com/Olivier-Boudeville/Ceylan-Seaplus/tree/master/tests/c-test/foobar)).

Please refer to the [Seaplus official documentation](http://seaplus.esperide.org), otherwise to its [mirror](http://olivier-boudeville.github.io/Ceylan-Seaplus/).

The 'master' branch corresponds to the current stable version of this layer.

Ceylan-Seaplus relies on [Ceylan-Myriad](https://github.com/Olivier-Boudeville/Ceylan-Myriad).
