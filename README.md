# SPARK_NORX

## Introduction

This is an Ada 2012 / SPARK 2014 project that implements the
[NORX](https://norx.io/) Authenticated Encryption with Additional Data
Algorithm , a second round candidate in the
[CAESAR](http://competitions.cr.yp.to/caesar.html) competition. NORX was
designed by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves.

The Ada code is written as a generic template, and each variant of NORX is
generated from the same code. There are some requirements of the Ada
implementation (for example, `Storage_Element` must be an 8-bit byte) and of
the generic parameters (for example the rate, key size etc must all be
multiples of the word size).

This project is free software (using the ISC permissive licence) and is
provided with no warranties, as set out in the file `LICENSE`.

## Overview of the packages

The main generic package is `NORX` which implements the high-level API as set
out in the NORX specification. This consists of just two procedures, `AEADEnc`
that `AEADDec`. The former procedure takes in a key `K`, a 'nonce' `N`, an
optional message header `A` (not encrypted), optional message to be encrypted
`M` and optional trailer `Z` (not encrypted) and returns the encrypted
ciphertext `C` and the authentication tag `T`. The latter procedure performs
the decryption and returns the decoded message and a boolean that indicates
whether the message was valid.

`NORX.Definitions` defines some constrained types used in the generic formal
parameters and `NORX.Load_Store` contains functions that convert between
standard word sizes and `Storage_Array` in Little-Endian format. Packages
`NORX6441` etc. are instantiations of this generic with the parameters
suggested by the NORX designers.

`NORX.Access_Internals` allows access to the lower-level API which allows you
to follow the internal state of the cipher as it processes some data.
`NORX.Utils` contains useful helper functions for printing out `Storage_Array`
and `State` types.

## Examples

Two example programs are included. `norx_example` is a simple example of using
the high-level API and demonstrates successful encryption and decryption, and
also unsuccessful decryption if the tag is altered.

`norx_test_vectors` uses the lower-level API to print the trace of a sample
encryption for each of the suggested variants of NORX. These can be compared
with the output from running `make debug` on the reference C code provided by
the NORX designers.

## Status of SPARK proof

All of the standard instantiations of `NORX` can be proved using SPARK for all
but one type of check. Currently SPARK cannot prove the full initialisation of
output arrays where this is done one element at a time in a loop rather than
in a single array expression. SPARK will therefore report unproved checks of
the form _'"C" might not be initialised'_ for each procedure with an array
output. While I believe there are no true errors of this type in the code it
is not appropriate to remove these proof errors. Simply zeroing the outputs at
the start of the procedures would enable simple proofs of initialisation, but
apart from being inefficient this would be hiding any lack of initialisation,
not fixing it.

However, SPARK is able to prove the absence of all other potential sources of
run-time exceptions, and proves that `AEADDec` will not return any decrypted
data if the tag verification failed. The GPL version of SPARK shipped with the
GNAT GPL 2015 Ada compiler from [AdaCore](http://libre.adacore.com/) was used
to develop this project.

## Project files and examples

Three project files for use with `GPRBuild` are provided. `spark_norx.gpr`
builds the NORX code as a static library. It takes an optional parameter
`mode` which can be one of `debug` or `optimise` (or its synonym `optimize`).
`spark_norx_external.gpr` covers the same code, but does not trigger rebuilds
of the library. `spark_norx_examples.gpr` builds the example code.

## Using GNATprove for verification

To verify the proofs (apart from array initialisation), the following command
can be used. The parallelism (`-j`) and time limit per proof (`--timeout`) may
need to be varied depending on the speed of your machine.

    gnatprove -P spark_norx.gpr -j 2 --timeout=3 --proof=progressive --warnings=continue

Alternatively, the `GPS` IDE can be used.
