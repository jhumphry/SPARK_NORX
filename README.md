# SPARK_NORX

## Introduction

This is an Ada 2012 / SPARK 2014 project that implements the
[NORX](https://norx.io/) Authenticated Encryption with Additional Data
Algorithm, a second round candidate in the
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
and `AEADDec`. The former procedure takes in a key `K`, a 'nonce' `N`, an
optional message header `A` (not encrypted), optional message to be encrypted
`M` and optional trailer `Z` (not encrypted) and returns the encrypted
cipher-text `C` and the authentication tag `T`. The latter procedure performs
the decryption and returns the decoded message and a Boolean that indicates
whether the message was valid. If any of `A`, `M` or `Z` parameters are not
used, the constant `Null_Storage_Array` can be passed to the routines.

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

As the code is written in SPARK, all of the standard instantiations of `NORX`
can be proved free of run-time exceptions for all but one type of check.
Currently SPARK cannot prove the full initialisation of output arrays where
this is done one element at a time in a loop rather than in a single array
expression. SPARK will therefore report unproved checks of the form _'"C"
might not be initialised'_ for each procedure with an array output. Simply
zeroing the output arrays at the start of the procedures would resolve these
unproved checks, but this would be inefficient and would hide any real errors.
Instead assertions of the form `C_Index = C'Last + 1` are proved at the end of
the procedures. These show that the whole array has been iterated over by the
time the procedure exits.

However, SPARK is able to prove the absence of all other potential sources of
run-time exceptions, and proves that `AEADDec` will not return any decrypted
data if the tag verification failed. The GPL SPARK prover `gnatprove` shipped
with the GNAT GPL 2015 Ada compiler from [AdaCore](http://libre.adacore.com/)
was used to develop this project.

## Project files and examples

Three project files for use with `GPRBuild` are provided. `spark_norx.gpr`
builds the NORX code as a static library. It takes two optional parameters:

- `mode` can be set to `debug` (the default) or `optimise`/`optimize`. This
sets appropriate compiler flags.

- `load_store` can be set to `explicit` (the default) or `le`. This setting
controls how arrays of bytes are converted into words. The `explicit` setting
compiles functions that use bit shifts and bit-wise operators to perform the
conversions. The `explicit` setting should work everywhere. The `le` setting
uses unchecked type conversions. This may be faster but requires a
Little-Endian machine and an Ada compiler which uses compatible machine
representation for the types.

`spark_norx_external.gpr` covers the same code, but does not trigger rebuilds
of the library. `spark_norx_examples.gpr` builds the example code.

## Using GNATprove for verification

To automatically verify the code (apart from array initialisation as discussed
above), the GPS IDE can be used. Alternatively the following commands can be
used at the shell. The parallelism (`-j`) and time limit per proof
(`--timeout`) may need to be varied depending on the speed of your machine.

    gnatprove -P spark_norx.gpr -j 2 --timeout=3 --proof=progressive --warnings=continue

Add `--report=all` if you want to see the checks that are proved as well.
