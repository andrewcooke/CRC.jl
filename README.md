# CRC

This is a [Julia](http://julialang.org/) module for calculating Cyclic
Redundancy Checksums (CRCs).  It also contains a commandline tool for
calculating the CRC of files.

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

## From the Command Line

The `crc.jl` file contains a simple utility for calculating the hash of files:

```
andrew@laptop:~/project/CRC> CRC=`find ~/.julia -name crc.jl`
andrew@laptop:~/project/CRC> julia $CRC -h
usage: crc.jl [-l] [-d] [-c CRC] [-a] [-h] [files...]

Calculate the CRC for the given files

positional arguments:
  files          the files to read (- for stdin)

optional arguments:
  -l, --list     list available CRC algorithms
  -d, --decimal  show checksums as decimal values (default is hex)
  -c, --crc CRC  name the CRC to use (default: CRC_32)
  -a, --append   append the data together
  -h, --help     show this help message and exit

andrew@laptop:~/project/CRC> julia $CRC -l | grep "CRC_32 "
CRC_32 width=32 poly=0x04c11db7 init=0xffffffff refin=true refout=true xorout=0xffffffff check=0xcbf43926
andrew@laptop:~/project/CRC> echo "123456789" > /tmp/crc.txt
andrew@laptop:~/project/CRC> julia $CRC /tmp/crc.txt
0x0e0117757 /tmp/crc.txt
```

TODO - why doesn't the above match the check value?


[![Build
Status](https://travis-ci.org/andrewcooke/CRC.jl.png)](https://travis-ci.org/andrewcooke/CRC.jl)
Julia 0.3 (trunk).

