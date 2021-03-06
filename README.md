# SPARK_NORX

## Introduction

This is an Ada 2012 / [SPARK 2014](http://spark-2014.org/) project that implements the
[NORX](https://norx.io/) Authenticated Encryption with Additional Data Algorithm, a third round
candidate in the [CAESAR](http://competitions.cr.yp.to/caesar.html) competition. NORX was designed
by Jean-Philippe Aumasson, Philipp Jovanovic and Samuel Neves.

The Ada code is written as a generic template, and each variant of NORX is generated from the same
code. There are some requirements of the Ada implementation (for example, `Storage_Element` must be
an 8-bit byte) and of the generic parameters (for example the rate, key size etc must all be
multiples of the word size).

This project targets version 3.0 of the specification for NORX32 and NORX64. The variants for
low-end systems are based on the separate paper 'NORX8 and NORX16: Authenticated Encryption for
Low-End Systems' by the same authors but with some modifications applied to maintain consistency
with the revisions made to the main specification.

This project is free software (using the ISC permissive licence) and is provided with no
warranties, as set out in the file `LICENSE`.

## Overview of the packages

The main generic package is `NORX` which implements the high-level API as set out in the NORX
specification. This consists of just two procedures, `AEADEnc` and `AEADDec`. The former procedure
takes in a key `K`, a 'nonce' `N`, an optional message header `A` (not encrypted), optional message
to be encrypted `M` and optional trailer `Z` (not encrypted) and returns the encrypted cipher-text
`C` and the authentication tag `T`. The latter procedure performs the decryption and returns the
decoded message and a Boolean that indicates whether the message was valid. If any of `A`, `M` or
`Z` parameters are not used, the constant `Null_Storage_Array` can be passed to the routines.

Packages `NORX6441` etc. are instantiations of this generic with the
parameters suggested by the NORX designers.

`NORX_Definitions` defines some constrained types used in the generic formal parameters and
`NORX_Load_Store` contains functions that convert between standard word sizes and `Storage_Array`
in Little-Endian format.

`NORX.Access_Internals` allows access to the lower-level API which allows you to follow the
internal state of the cipher as it processes some data. `NORX.Utils` contains useful helper
functions for printing out `Storage_Array` and `State` types.

## Examples

Three example programs are included. `norx_example` is a simple example of using the high-level API
and demonstrates successful encryption and decryption, and also unsuccessful decryption if the tag
is altered.

`norx_test_vectors` uses the lower-level API to print the trace of a sample encryption for each of
the suggested variants of NORX. These can be compared with the output from running `make debug` on
the reference C code provided by the NORX designers.

`norx_check_padding` checks that authenticated encryption and decryption works correctly when the
lengths of the header, message and trailer inputs vary. This is primarily to check for any bugs in
the implementation of the padding.

## Status of SPARK proof

As the code is written in SPARK, all of the standard instantiations of `NORX` can be proved free of
run-time exceptions for all but one type of check. Currently SPARK cannot prove the full
initialisation of output arrays where this is done one element at a time in a loop rather than in a
single array expression. SPARK will therefore report unproved checks of the form _'"C" might not be
initialised'_ for each procedure with an array output. Simply zeroing the output arrays at the
start of the procedures would resolve these unproved checks, but this would be inefficient and
would hide any real errors. Instead assertions of the form `C_Index = C'Last + 1` are proved at the
end of the procedures. These show that the whole array has been iterated over by the time the
procedure exits. `pragma Annotate` has been used to justify the output array initialisation for
these procedures.

However, SPARK GPL 2016, SPARK Discovery GPL 2017 and GNAT Community 2018 are able to prove the
absence of all other potential sources of run-time exceptions, which amount to 98% of the checks,
without manual intervention. It also proves that `AEADDec` will not return any decrypted data if
the tag verification failed. The GPL SPARK prover `gnatprove` shipped with GNAT Community 2018 from
[AdaCore](https://www.adacore.com/community) is primarily used for this project.

## Project files

Three project files for use with `GPRBuild` are provided. `spark_norx.gpr` builds the NORX code as
a static library. It takes two optional parameters:

- `mode` can be set to `debug` (the default) or `optimise`/`optimize`. This sets appropriate
compiler flags.

- `load_store` can be set to `explicit` (the default) or `le`. This setting controls how arrays of
bytes are converted into words. The `explicit` setting compiles functions that use bit shifts and
bit-wise operators to perform the conversions. The `explicit` setting should work everywhere. The
`le` setting uses unchecked type conversions. This may be faster but requires a Little-Endian
machine and an Ada compiler which uses compatible machine representation for the types.

`spark_norx_external.gpr` covers the same code, but does not trigger rebuilds of the library.
`spark_norx_examples.gpr` builds the example code.

## Using GNATprove for verification

To automatically verify the code (apart from array initialisation as discussed above), the GPS IDE
can be used. Alternatively the following commands can be used at the shell. The settings may need
to be varied depending on the speed of your machine.

- SPARK GPL 2016

    gnatprove -P spark_norx.gpr -U -j0 --level=0 --proof=progressive --warnings=continue
    
- SPARK Discovery GPL 2017

    gnatprove -P spark_norx.gpr -Xload_store=explicit -Xmode=debug -j0 --level=2

- GNAT Community 2018

    gnatprove -P spark_norx.gpr -j0 -Xload_store=explicit -Xmode=debug --level=0

Add `--report=all` if you want to see the checks that are proved as well.

For SPARK Discovery GPL 2017 the built-in SMT solver, Alt-Ergo, may not be able to prove all of the
VC. Add the alternative Z3 and/or CVC4 provers as explained in the SPARK user guide.

## Acknowledgements

Thanks to Philipp Jovanovic (NORX team), Claire Dross (AdaCore) and Yannick Moy (AdaCore) for
helpful comments and suggestions.
