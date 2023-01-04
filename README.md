[![Build Status](https://travis-ci.org/andrewcooke/CRC.jl.png)](https://travis-ci.org/andrewcooke/CRC.jl)
[![Coverage Status](https://coveralls.io/repos/andrewcooke/CRC.jl/badge.svg)](https://coveralls.io/r/andrewcooke/CRC.jl)

# CRC

This is a [Julia](http://julialang.org/) module for calculating Cyclic
Redundancy Checksums (CRCs).

* All the algorithms in the [RevEng
  Catalogue](http://reveng.sourceforge.net/crc-catalogue) are supported.

* New algorithms can be easily added.

* Calculation can be direct or via cached tables.

* Only arrays of bytes are accepted as data (it's certainly possible
  to handle arbitrary sized sequences; previous versions did this, but
  it complicated the code for little practical gain so I removed it -
  please contact me if you want me to add it back).

Although the performance of CRC.jl is good, even faster CRC-32 checksums are possible using the
[heavily optimized C implementation](https://github.com/madler/zlib/blob/04f42ceca40f73e2978b50e93806c2a18c1281fc/crc32.c) in zlib
via the [CRC32.jl package](https://github.com/JuliaIO/CRC32.jl), or using hardware-accelerated CRC-32c checksums in
the [CRC32c standard library](https://docs.julialang.org/en/v1/stdlib/CRC32c/).

## Examples

### Calculate a CRC-32 Sum

```
julia> using CRC

julia> crc32 = crc(CRC_32)
(anonymous function)

julia> crc32("123456789")
0xcbf43926
```

The function `crc()` constructs a lookup table, which is cached in the
returned function (here, `crc32()`).  Re-using `crc32()` to calculate a series
of CRCs is therefore more efficient than starting with `crc()` each time.

The returned function can also be called with a file handle, so calling
`open(crc32, file)` will return the checksum of `file`.

### Within Your Program

The same example as above, but inside your program, would look like this:

```
using CRC
...
crc32 = crc(CRC_32)  # create our own crc function, just once
...
function some_func(...)
    ...
    x = crc32(data)  # use the crc function created above, many times
    ...
end
...
```

### Force Direct (Tableless) Calculation

```
julia> crc(CRC_32, tables=NoTables)("123456789")
0xcbf43926
```

### Define an Algorithm

For example,
[CRC-7](http://reveng.sourceforge.net/crc-catalogue/1-15.htm#crc.cat-bits.7),
catalogued as `width=7 poly=0x09 init=0x00 refin=false refout=false
xorout=0x00 check=0x75 name="CRC-7"`

```
julia> myCRC7 = spec(7, 0x09, 0x00, false, false, 0x00, 0x75)
CRC.Spec{UInt8}(7, 0x09, 0x00, false, false, 0x00, 0x75)

julia> @assert crc(myCRC7)(CHECK) == myCRC7.check
```

Of course, this is already defined:

```
julia> CRC_7
CRC.Spec{UInt8}(7, 0x09, 0x00, false, false, 0x00, 0x75)
```

## Installation

Julia can be downloaded [here](http://julialang.org/downloads/).  Once
Julia is working you can install this package using:

```
julia> Pkg.add("CRC")
```

## Versions

* 4.0.0 - 2020-11-27 Remove command line functionality.

* 3.1.0 - 2020-11-07 Update ArgParse dependency and add Project.toml.

* 3.0.0 - 2018-02-28 Update for Julia 1.0

* 1.2.0 - 2016-09-28 Drop Julia 0.3 support and switch to Libz.

* 1.1.0 - 2015-06-09 Small fixes for Julia 0.4, Travis + Coverage.

* 1.0.0 - 2014-06-31 Changed handler method so that a `String` is converted to
  bytes (instead of being treated as a file path).  This *will break* existing
  code that uses the current handler (sorry!), but I hope I don't have many
  users (particularly users that are calling that method)!

* 0.2.0 - Initial release(s).
