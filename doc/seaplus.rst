
.. _Top:


.. title:: Welcome to the Ceylan-Seaplus 0.1 documentation

.. comment stylesheet specified through GNUmakefile


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex

.. comment Would appear too late, can only be an be used only in preamble:
.. comment :raw-latex:`\usepackage{graphicx}`
.. comment As a result, in this document at least a '.. figure:: XXXX' must
.. exist, otherwise: 'Undefined control sequence \includegraphics.'.


:raw-html:`<a name="seaplus_top"></a>`

:raw-html:`<div class="banner"><p><em>Seaplus 0.1 documentation</em> <a href="http://seaplus.esperide.org">browse latest</a> <a href="https://olivier-boudeville.github.io/Ceylan-Seaplus/seaplus-0.1.html">browse mirror</a> <a href="seaplus-0.1.pdf">get PDF</a> <a href="#seaplus_top">go to top</a> <a href="#seaplus_bottom">go to bottom</a> <a href="mailto:about(dash)seaplus(at)esperide(dot)com?subject=[Ceylan-Seaplus 0.1]%20Remark">email us</a></p></div>`



:raw-html:`<center><img src="seaplus-title.png" width="50%"></img></center>`
:raw-latex:`\includegraphics[scale=0.30]{seaplus-title.png}`




----------------------------------------------------------------
Seaplus: Streamlining a safe execution of C/C++ code from Erlang
----------------------------------------------------------------


:Organisation: Copyright (C) 2018-2019 Olivier Boudeville
:Contact: about (dash) seaplus (at) esperide (dot) com
:Creation Date: Saturday, February 2, 2019
:Lastly Updated: Saturday, February 2, 2019
:Dedication: Users and maintainers of the ``Seaplus`` bridge, version 0.1.
:Abstract:

	The role of the ``Seaplus`` bridge is to control C or C++ code from Erlang, not as NIF but thanks to a port, and to streamline the corresponding integration process.


.. meta::
   :keywords: Seaplus, C, C++, integration, interface, bridge, Erlang


The latest version of this documentation is to be found at the `official Seaplus website <http://seaplus.esperide.org>`_ (``http://seaplus.esperide.org``).

:raw-html:`This Seaplus documentation is also available in the PDF format (see <a href="seaplus-0.1.pdf">seaplus.pdf</a>), and mirrored <a href="http://olivier-boudeville.github.io/Ceylan-Seaplus/seaplus-0.1.html">here</a>.`

:raw-latex:`The documentation is also mirrored \href{https://olivier-boudeville.github.io/Ceylan-Seaplus/seaplus-0.1.html}{here}.`




:raw-latex:`\pagebreak`



.. _`table of contents`:


.. contents:: Table of Contents
  :depth: 3


:raw-latex:`\pagebreak`


Overview
========

A typical use-case is having a C or C++ library of interest that we would like be able to use from Erlang, whereas, for any reason (availability of sources, complexity, size or interest), recoding it (in Erlang) is not desirable.

However tempting it may be to integrate tightly C/C++ code to the Erlang VM (typically through a `NIF <http://erlang.org/doc/tutorial/nif.html>`_), one may prefer trading maximum performances for safety, and run that C/C++ code (often at last partly foreign) into a separate, isolated (operating system) process.

Then the integrated code will not be able to crash the Erlang application, and for example any memory leak it would induce would only affect its own process - not the application one.

Indeed, taking into account the Erlang `Interoperability Tutorial <http://erlang.org/doc/tutorial/users_guide.html>`_, the following approaches are the most commonly considered ones when having to make C/C++ code available from Erlang:

- raw **ports** and **linked-in drivers**: they are mostly obsolete for the task at hand (superseded by better counterparts)
- ``os:cmd/1``: a rudimentary solution that offers little control and requires much syntactic parsing effort
- custom **socket-based protocol**: quite low-level and complicated
- ``NIF``: as mentioned, they may jeopardise the VM (this is acceptable or not)
- `C-Node <http://erlang.org/doc/tutorial/cnode.html>`_ and  `Erl_Interface <http://erlang.org/doc/tutorial/erl_interface.html>`_: this is the combination that we preferred for Seaplus

In a nutshell, this approach consists on spawning a "fake" Erlang node written in C (the ``C-Node``) and using the standard *Erlang external term format* in order to communicate with it (relying for that on the ``Erl_Interface`` facility). Doing so allows a seamless communication to happen, despite language heterogeneity.

C-Node and Erl_Interface help a lot, yet, as shown in `this example <http://erlang.org/doc/tutorial/erl_interface.html#erlang-program>`_, quite a lot of boiler-plate/bridging code (home-made encoding and conventions) remains needed.

The **goal of Seaplus is to reduce that interfacing effort**, thanks to a set of generic, transverse functions on either side (modules in Erlang, a library in C/C++) and the use of metaprogramming (i.e. the Seaplus parse transform) in order to generate at least a part of the code needed in both sides, while leaving to the developer enough leeway so that he can define the mapping interface he prefers (ex: with regards to naming, types introduced and used, etc.).

``Ceylan-Seaplus`` relies on various facilities offered by the `Ceylan-Myriad <http://myriad.esperide.org>`_ toolbox.



Usage
=====


So we have a (possibly third-party) service (typically a library, directly usable from C, offering a set of functions) that we want to integrate, i.e. to make available from Erlang.

Let's suppose that said service is named ``Foobar``, and that these functions, on the C side, are declared as (typically in some ``foobar.h`` header file, referring to a possibly opaque ``foobar.so`` library):

.. code:: c

struct s ;

enum foo_status { low_speed, medium_speed, high_speed } ;

int foo( int a ) ;
struct s * bar( double a, foo_status status ) ;
const char * baz( void * p ) ;
bool tur() ;
void frob( void * p ) ;

  int foo(int a);
  struct s * bar(double a, foo_status);
  const char * baz(void * p);
  bool tur();
  void frob(void * p);


With the definition of this example, we ensured to reproduce real-life situations, like atoms vs enums, dynamic memory allocation (for the returned struct) and runtime failures (calling ``foo(0)`` will trigger a division by zero).

What would be the corresponding ideal Erlang interface to make such a fantastic service available?

First of all, multiple corresponding APIs can be considered, and some design choices have to be made (we can foresee some are more elegant/convenient than others, and that a perfect, universal, one-size-fit-all automated mapping does not seem so achievable).

An easy step is to decide to map each of these C functions to an Erlang counterpart function that, unsurprisingly, bears the same name and most of the time has the same arity, and to have them gathered into a single module that would be best named ``foobar`` (thus to be defined in ``foobar.erl``).

So we believe that, in order to rely on a convenient Erlang-side API for this service, adaptations have to be made (ex: with regard to typing), and thus that it should preferably be defined in an ad-hoc manner (i.e. tailor-made, not automatically through a mapping suffering from impedance mismatch). So this service-specific API shall be devised by the service integrator (i.e. the developer in charge of the integration of the C/C++ code to Erlang); but how?

At the very least, what will be offered on the Erlang side by our ``foobar`` module shall be somehow specified. A very appropriate way of doing so is to list the `type specifications <http://erlang.org/doc/reference_manual/typespec.html>`_ of the targeted counterpart functions meant to be available (defined and exported), like in [#]_:

.. code:: erlang

 -module(foobar).

 -spec foo(integer()) -> integer().
 -spec bar(float(),atom()) -> some_foo_record().
 -spec baz(any()) -> maybe(text_utils:ustring()).
 -spec tur() -> bool().
 -spec frob(term()) -> void().

.. [#] Note that some types are introduced thanks to the use of Myriad - this does not matter here.

Comments (description, usage, examples) are also expected to be joined to these specs, they were omitted here for brevity.


Other facility functions that all integrated services will need, and whose signature (if not implementation) would not differ from a service to another (ex: to start/stop this service from Erlang), will also certainly be needed. However listing these facility functions in our ``foobar`` module would offer little interest (as they are the same for all integrated services), so these extra functions are to remain implicit here [#]_.

The list of these built-in, auto-defined Seaplus functions is:

- ``start/{0,1,2}`` and ``start_link/{0,1,2}``
- ``stop/{0,1}``

.. [#] Note though that, at least for some services, specific initialisation/tear-down functions may exist in the vanilla, C version of that service. In that case, they should be added among said function specifications (preferably named for example ``init``/``teardown`` or alike, in order to distinguish from the Seaplus-reserved ``start``/``stop`` primitives).


Of course such a module, as it was defined above (i.e. just a set of function specifications), is useless. But the Seaplus parse transform will automatically enrich and transform it so that, once the C part will be available, the ``Foobar`` service will become fully usable from Erlang.

More precisely, for each of the function type specification, a corresponding bridging implementation will be generated and added (unless the ``foobar`` module already includes one, so that the user can selectively disable the Seaplus code generation), whilst the facility functions will be included as well.

Here is a corresponding (mostly meaningless) usage example of this ``foobar`` module, when executed from any given process (ex: a test one):

.. code:: erlang

  foobar:start_link(),
  MyFooRecord = foobar:bar(3.14,full_speed),
  NewCount = foobar:foo(MyFooRecord#some_foo_record.count),
  Res = case foobar:tur() of
	true ->
	  foobar:baz(NewCount,"Hello");
	false ->
	  non_tur
  end,
  io:format("Having: ~s~n",[foobar:frob(Res)]),
  foobar:stop().


At this point, one may think that, thanks to these function specs, the counterpart C bridging code may have been generated in the same movement? Unfortunately, no (at least, not yet): C-side elements will have been produced by the Seaplus parse-transform (notably the function selector include, used to map functions on either sides), but the conversion (thanks to ``Erl_Interface``) from the Erlang terms received by the port into arguments that will feed the C functions and on the other way round (i.e. from the C results to the Erlang terms that shall be sent back) is still to be written by the service integrator.



Mode of Operation
=================

It is mostly the one described in the `Erl_Interface <http://erlang.org/doc/tutorial/erl_interface.html>`_, once partly automated (Erlang and C code generation) and adapted for increased performances (notably: no extra relay process between the user code and the port).
