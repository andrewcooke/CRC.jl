# CRC

This is a [Julia](http://julialang.org/) module for calculating Cyclic
Redundancy Checksums (CRCs).

* All the algoirthms in the [Reveng
  Catalogue](http://reveng.sourceforge.net/crc-catalogue) are supported.

* CRCs can be calculated for sequences of integers of any common size 
  (from 8bit bytes to 128bit long long words).

* Calculation can be direct or via tabules.

* Speed is currently about 4x *slower* than optimized C (eg when
  compared to the CRC32 zlib implementation).



[![Build
Status](https://travis-ci.org/andrewcooke/CRC.jl.png)](https://travis-ci.org/andrewcooke/CRC.jl)
Julia 0.3 (trunk).

