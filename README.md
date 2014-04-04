# CRC

This is a [Julia](http://julialang.org/) module for calculating Cyclic
Redundancy Checksums (CRCs).

* All the algoirthms in the [RevEng
  Catalogue](http://reveng.sourceforge.net/crc-catalogue) are supported.

* New algorithms can be easilly added.

* CRCs can be calculated for sequences of integers of any common size
  (from 8 bit bytes to 128 bit long long words).

* Calculation can be direct or via tables.

* Table sizes (number of index bits) can be specified (default 8 bits).

* Speed is currently about 3-4x *slower* than optimized C (eg when
  compared to the CRC32 zlib implementation).

## Examples

### Calculate a CRC32 Hash

```
julia> using CRC

julia> crc(CRC_32, b"123456789")
0xcbf43926
```

### Use a Cached Table

```
julia> c = crc(CRC_32, Uint8)
(anonymous function)

julia> @time c(b"123456789")
elapsed time: 0.091254649 seconds (5219272 bytes allocated)
0xcbf43926

julia> @time c(b"123456789")
elapsed time: 6.9576e-5 seconds (1136 bytes allocated)
0xcbf43926
```

### Calculate Directly

```
julia> crc(CRC_32, b"123456789", table=false)
0xcbf43926
```

### Define an Algorithm

For example,
[CRC-7](http://reveng.sourceforge.net/crc-catalogue/1-15.htm#crc.cat-bits.7),
catalogued as `width=7 poly=0x09 init=0x00 refin=false refout=false
xorout=0x00 check=0x75 name="CRC-7"`

```
julia> myCRC7 = Spec(7, 0x09, 0x00, false, false, 0x00, 0x75)
Spec{Uint8}(7,0x09,0x00,false,false,0x00,0x75)

julia> @assert crc(myCRC7, TEST) == myCRC7.test
```

Of course, this is already defined:

```
julia> CRC_7
Spec{Uint8}(7,0x09,0x00,false,false,0x00,0x75)
```


[![Build
Status](https://travis-ci.org/andrewcooke/CRC.jl.png)](https://travis-ci.org/andrewcooke/CRC.jl)
Julia 0.3 (trunk).

