# Ceylan-Seaplus
Part of the Ceylan project gathering facilities to better integrate C/C++ code to Erlang.

Note: this project is not functional yet (but in progress).

This binding relies on a port (not a NIF) based on the standard Erl_Interface, so that the integrated C/C++ code cannot jeopardise the operation of the Erlang VM (hence at the cost of a few message exchanges, marshalling/demarshalling having to happen anyway).

A parse transform is used in order to ease the definition of the Erlang-side of the driver.

A complete, functional example of a service to integrate is given.

Ceylan-Seaplus relies on [Ceylan-Myriad](https://github.com/Olivier-Boudeville/Ceylan-Myriad).
