# CRC

This is a [Julia](http://julialang.org/) module for calculating Cyclic
Redundancy Checksums (CRCs).

* All the algoirthms in the [RevEng
  Catalogue](http://reveng.sourceforge.net/crc-catalogue) are supported.

* New algorithms can be easily added.

* Calculation can be direct or via cached tables.

* Speed is comparable to optimized C (checked against CRC32 in libz).

* Only arrays of bytes are accepted as data (it's certainly possible
  to handle arbitrary sized sequences; previous versions did this, but
  it complicated the code for little practical gain so I removed it -
  please contact me if you want me to add it back).

## Examples

### Calculate a CRC-32 Hash

```
julia> using CRC

julia> c = crc(CRC_32)
(anonymous function)

julia> c(b"123456789")
0xcbf43926
```

The function `crc()` constructs a lookup table, which is cached in the
returned function (here, `c()`).  Re-using `c()` to calculate a series
of CRCs is therefore more efficient than starting with `crc()` each
time.

### Force Direct (Tableless) Calculation

```
julia> crc(CRC_32, tables=NoTables)(b"123456789")
0xcbf43926
```

### Define an Algorithm

For example,
[CRC-7](http://reveng.sourceforge.net/crc-catalogue/1-15.htm#crc.cat-bits.7),
catalogued as `width=7 poly=0x09 init=0x00 refin=false refout=false
xorout=0x00 check=0x75 name="CRC-7"`

```
julia> myCRC7 = spec(7, 0x09, 0x00, false, false, 0x00, 0x75)
Spec{Uint8}(7,0x09,0x00,false,false,0x00,0x75)

julia> @assert crc(myCRC7)(TEST) == myCRC7.test
```

Of course, this is already defined:

```
julia> CRC_7
Spec{Uint8}(7,0x09,0x00,false,false,0x00,0x75)
```


[![Build
Status](https://travis-ci.org/andrewcooke/CRC.jl.png)](https://travis-ci.org/andrewcooke/CRC.jl)
Julia 0.3 (trunk).

