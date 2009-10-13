Getopt for Erlang
=================

Command-line parsing module that uses a syntax similar to that of GNU Getopt.


Requirements
------------

You should only need a somewhat recent version of Erlang/OTP though the module
has only been tested with Erlang R13B.


Installation
------------

To compile the module you simply run 'make'.
To run the unit tests run 'make test'.
To build the (very) limited documentation run 'make docs'.


Known limitations
-----------------

  - The syntax for multiple short options with no argument (e.g. -abc) is not
    supported yet.
  - The syntax for non-option arguments that start with '-' (e.g. -a -- -b)
    is not supported yet.
