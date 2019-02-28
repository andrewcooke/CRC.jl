[![Build Status](https://travis-ci.org/andrewcooke/CRC.jl.png)](https://travis-ci.org/andrewcooke/CRC.jl)
[![Coverage Status](https://coveralls.io/repos/andrewcooke/CRC.jl/badge.svg)](https://coveralls.io/r/andrewcooke/CRC.jl)

# CRC

This is a [Julia](http://julialang.org/) module for calculating Cyclic
Redundancy Checksums (CRCs).  It also contains a [command
line](#from-the-command-line) tool for calculating the CRC of files.

* All the algorithms in the [RevEng
  Catalogue](http://reveng.sourceforge.net/crc-catalogue) are supported.

* New algorithms can be easily added.

* Calculation can be direct or via cached tables.

* Speed is comparable to optimized C (checked against CRC32 in libz).

* Only arrays of bytes are accepted as data (it's certainly possible
  to handle arbitrary sized sequences; previous versions did this, but
  it complicated the code for little practical gain so I removed it -
  please contact me if you want me to add it back).

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

## From the Command Line

The `main(ARGS)` function is a simple utility for calculating the
checksum of files:

```
andrew@laptop:~/project/CRC> julia -e "using CRC; main(ARGS)" -- -h
usage: <PROGRAM> [-l] [-d] [-c CRC] [-a] [-h] [files...]

Calculate the CRC for the given files

positional arguments:
  files          the files to read (- for stdin)

optional arguments:
  -l, --list     list available CRC algorithms
  -d, --decimal  show checksums as decimal values (default is hex)
  -c, --crc CRC  name the CRC to use (default: CRC_32)
  -a, --append   combine the data from all files
  -h, --help     show this help message and exit

andrew@laptop:~/project/CRC> alias crc='julia -e "using CRC; main(ARGS)" -- '
andrew@laptop:~/project/CRC> crc -l | grep "CRC_32"
CRC_32_XFER width=32 poly=0x000000af init=0x00000000 refin=false refout=false xorout=0x00000000 check=0xbd0be338
CRC_32_POSIX width=32 poly=0x04c11db7 init=0x00000000 refin=false refout=false xorout=0xffffffff check=0x765e7680
...
andrew@laptop:~/project/CRC> echo -n "123456789" > /tmp/crc.txt
andrew@laptop:~/project/CRC> crc /tmp/crc.txt
0xcbf43926 /tmp/crc.txt
```

Note that the result matches the `check` value for the algorithm.

## Installation

Julia can be downloaded [here](http://julialang.org/downloads/).  Once
Julia is working you can install this package using:

```
julia> Pkg.add("CRC")
```

Then, to define the `crc` command line utility:

```
alias crc='julia -e "using CRC; main(ARGS)" -- '
```

(in, for example, `.alias`).

## Other CRC Packages

* [CRC32.jl](https://github.com/fhs/CRC32.jl) is a simple implementation of
  CRC_32 - it is easier to understand than this code, but slower, and only
  supports a single CRC algorithm.

* [libz](https://github.com/dcjones/Zlib.jl) includes (amongst many
  other things) a wrapper to a C version of CRC_32 - it is similar in
  speed to this package (and wil be faster for short, single uses,
  because the lookup table is pre-calculated), but only supports that
  single algorithm.

## Versions

* 3.0.0 - 2018-02-28 Update for Julia 1.0

* 1.2.0 - 2016-09-28 Drop Julia 0.3 support and switch to Libz.

* 1.1.0 - 2015-06-09 Small fixes for Julia 0.4, Travis + Coverage.

* 1.0.0 - 2014-06-31 Changed handler method so that a `String` is converted to
  bytes (instead of being treated as a file path).  This *will break* existing
  code that uses the current handler (sorry!), but I hope I don't have many
  users (particularly users that are calling that method)!

* 0.2.0 - Initial release(s).
