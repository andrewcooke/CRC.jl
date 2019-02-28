export crc, spec, CHECK, NoTables, Single, Multiple, ALL,
       CRC_3_ROHC, CRC_4_ITU, CRC_5_EPC, CRC_5_ITU, CRC_5_USB,
       CRC_6_CDMA2000_A, CRC_6_CDMA2000_B, CRC_6_DARC, CRC_6_ITU,
       CRC_7, CRC_7_ROHC, CRC_8, CRC_8_CDMA2000, CRC_8_DARC,
       CRC_8_DVB_S2, CRC_8_EBU, CRC_8_I_CODE, CRC_8_ITU, CRC_8_MAXIM,
       CRC_8_ROHC, CRC_8_WCDMA, CRC_10, CRC_10_CDMA2000, CRC_11,
       CRC_12_3GPP, CRC_12_CDMA2000, CRC_12_DECT, CRC_13_BBC,
       CRC_14_DARC, CRC_15, CRC_15_MPT1327, CRC_16_ARC,
       CRC_16_AUG_CCITT, CRC_16_BUYPASS, CRC_16_CCITT_FALSE,
       CRC_16_CDMA2000, CRC_16_DDS_110, CRC_16_DECT_R, CRC_16_DECT_X,
       CRC_16_DNP, CRC_16_EN_13757, CRC_16_GENIBUS, CRC_16_MAXIM,
       CRC_16_RIELLO, CRC_16_TELEDISK, CRC_16_USB, CRC_16_CRC_A,
       CRC_16_KERMIT, CRC_16_MODBUS, CRC_16_X_25, CRC_16_XMODEM,
       CRC_24, CRC_24_FLEXRAY_A, CRC_24_FLEXRAY_B, CRC_31_PHILIPS,
       CRC_32, CRC_32_BZIP2, CRC_32_C, CRC_32_D, CRC_32_MPEG_2,
       CRC_32_POSIX, CRC_32_Q, CRC_32_JAMCRC, CRC_32_XFER, CRC_40_GSM,
       CRC_64, CRC_64_WE, CRC_64_XZ, CRC_82_DARC

import Base.Cartesian: @nexprs
import Base: ==, isless, print

const U = Unsigned

struct UnalignedVector{T<:U}
    a::Vector{UInt8}
end
# unsafe
@inline Base.getindex(a::UnalignedVector{T}, i) where {T} = unsafe_load(Ptr{T}(pointer(a.a)), i)
@inline Base.length(a::UnalignedVector{T}) where {T} = length(a.a) ÷ sizeof(T)
@inline unaligned_reinterpret(::Type{T}, a::Vector{UInt8}) where {T<:U} = UnalignedVector{T}(a)
@inline unaligned_reinterpret(::Type{UInt8}, a::Vector{UInt8}) = a
MaybeUnalignedVector{T} = Union{UnalignedVector{T},Vector{T}}

# ---- CRC specifications from http://www.zlib.net/crc_v3.txt


CHECK = Vector{UInt8}(b"123456789")   # universal check vector

struct Spec{P<:U}
    width::Int    # polynomial degree
    poly::P       # generating poly with msb missing
    init::P       # initial remainder
    refin::Bool   # reflect input
    refout::Bool  # reflect output
    xorout::P     # xored with final remainder
    check::P      # checksum for CHECK
    function (::Type{Spec{P}})(width::Int, poly::P, init::P, refin::Bool, refout::Bool, xorout::P, check::P) where {P}
        @assert width <= 8 * sizeof(P)
        new{P}(width, poly, init, refin, refout, xorout, check)
    end
end

spec(poly::P, init::P, refin::Bool, refout::Bool, xorout::P, check::P) where {P<:U} =
    Spec{P}(8*sizeof(P), poly, init, refin, refout, xorout, check)

spec(width::Int, poly::P, init::P, refin::Bool, refout::Bool, xorout::P, check::P) where {P<:U} =
    Spec{P}(width, poly, init, refin, refout, xorout, check)

==(a::Spec, b::Spec) = a.width == b.width && a.poly == b.poly && a.init == b.init && a.refin == b.refin && a.refout == b.refout && a.xorout == b.xorout

isless(a::Spec, b::Spec) = a.width < b.width || (a.width == b.width && (a.poly < b.poly || a.poly == b.poly && (a.init < b.init || (a.init == b.init && !a.refin && (a.refin != b.refin || (a.refin == b.refin && !a.refout && (a.refout != b.refout || a.xorout < b.xorout)))))))

function print(io::IO, s::Spec{P}) where {P<:U}
    n = 1 + div(s.width - 1, 4)
    h = x -> "0x" * string(x, base = 16, pad = n)
    print(io, "width=$(s.width) poly=$(h(s.poly)) init=$(h(s.init)) refin=$(s.refin) refout=$(s.refout) xorout=$(h(s.xorout)) check=$(h(s.check))")
end


# http://reveng.sourceforge.net/crc-catalogue/1-15.htm
CRC_3_ROHC =         spec(3, 0x03, 0x07, true,  true,  0x00, 0x06)
CRC_4_ITU =          spec(4, 0x03, 0x00, true,  true,  0x00, 0x07)
CRC_5_EPC =          spec(5, 0x09, 0x09, false, false, 0x00, 0x00)
CRC_5_ITU =          spec(5, 0x15, 0x00, true,  true,  0x00, 0x07)
CRC_5_USB =          spec(5, 0x05, 0x1f, true,  true,  0x1f, 0x19)
CRC_6_CDMA2000_A =   spec(6, 0x27, 0x3f, false, false, 0x00, 0x0d)
CRC_6_CDMA2000_B =   spec(6, 0x07, 0x3f, false, false, 0x00, 0x3b)
CRC_6_DARC =         spec(6, 0x19, 0x00, true,  true,  0x00, 0x26)
CRC_6_ITU =          spec(6, 0x03, 0x00, true,  true,  0x00, 0x06)
CRC_7 =              spec(7, 0x09, 0x00, false, false, 0x00, 0x75)
CRC_7_ROHC =         spec(7, 0x4f, 0x7f, true,  true,  0x00, 0x53)
CRC_8 =              spec(0x07, 0x00, false, false, 0x00, 0xf4)
CRC_8_CDMA2000 =     spec(0x9b, 0xff, false, false, 0x00, 0xda)
CRC_8_DARC =         spec(0x39, 0x00, true,  true,  0x00, 0x15)
CRC_8_DVB_S2 =       spec(0xd5, 0x00, false, false, 0x00, 0xbc)
CRC_8_EBU =          spec(0x1d, 0xff, true,  true,  0x00, 0x97)
CRC_8_I_CODE =       spec(0x1d, 0xfd, false, false, 0x00, 0x7e)
CRC_8_ITU =          spec(0x07, 0x00, false, false, 0x55, 0xa1)
CRC_8_MAXIM =        spec(0x31, 0x00, true,  true,  0x00, 0xa1)
CRC_8_ROHC =         spec(0x07, 0xff, true,  true,  0x00, 0xd0)
CRC_8_WCDMA =        spec(0x9b, 0x00, true,  true,  0x00, 0x25)
CRC_10 =             spec(10, 0x0233, 0x0000, false, false, 0x0000, 0x0199)
CRC_10_CDMA2000 =    spec(10, 0x03d9, 0x03ff, false, false, 0x0000, 0x0233)
CRC_11 =             spec(11, 0x0385, 0x001a, false, false, 0x0000, 0x05a3)
CRC_12_3GPP =        spec(12, 0x080f, 0x0000, false, true,  0x0000, 0x0daf)
CRC_12_CDMA2000 =    spec(12, 0x0f13, 0x0fff, false, false, 0x0000, 0x0d4d)
CRC_12_DECT =        spec(12, 0x080f, 0x0000, false, false, 0x0000, 0x0f5b)
CRC_13_BBC =         spec(13, 0x1cf5, 0x0000, false, false, 0x0000, 0x04fa)
CRC_14_DARC =        spec(14, 0x0805, 0x0000, true,  true,  0x0000, 0x082d)
CRC_15 =             spec(15, 0x4599, 0x0000, false, false, 0x0000, 0x059e)
CRC_15_MPT1327 =     spec(15, 0x6815, 0x0000, false, false, 0x0001, 0x2566)

# http://reveng.sourceforge.net/crc-catalogue/16.htm#crc.cat.crc-16-ccitt-false
CRC_16_ARC =         spec(0x8005, 0x0000, true,  true,  0x0000, 0xbb3d)
CRC_16_AUG_CCITT =   spec(0x1021, 0x1d0f, false, false, 0x0000, 0xe5cc)
CRC_16_BUYPASS   =   spec(0x8005, 0x0000, false, false, 0x0000, 0xfee8)
CRC_16_CCITT_FALSE = spec(0x1021, 0xffff, false, false, 0x0000, 0x29b1)
CRC_16_CDMA2000 =    spec(0xc867, 0xffff, false, false, 0x0000, 0x4c06)
CRC_16_DDS_110 =     spec(0x8005, 0x800d, false, false, 0x0000, 0x9ecf)
CRC_16_DECT_R =      spec(0x0589, 0x0000, false, false, 0x0001, 0x007e)
CRC_16_DECT_X =      spec(0x0589, 0x0000, false, false, 0x0000, 0x007f)
CRC_16_DNP =         spec(0x3d65, 0x0000, true,  true,  0xffff, 0xea82)
CRC_16_EN_13757 =    spec(0x3d65, 0x0000, false, false, 0xffff, 0xc2b7)
CRC_16_GENIBUS =     spec(0x1021, 0xffff, false, false, 0xffff, 0xd64e)
CRC_16_MAXIM =       spec(0x8005, 0x0000, true,  true,  0xffff, 0x44c2)
CRC_16_RIELLO =      spec(0x8bb7, 0x0000, false, false, 0x0000, 0xd0db)
CRC_16_TELEDISK =    spec(0x1021, 0x89ec, true,  true,  0x0000, 0x26b1)
CRC_16_USB =         spec(0x8005, 0xffff, true,  true,  0xffff, 0xb4c8)
CRC_16_CRC_A =       spec(0x1021, 0xc6c6, true,  true,  0x0000, 0xbf05)
CRC_16_KERMIT =      spec(0x1021, 0x0000, true,  true,  0x0000, 0x2189)
CRC_16_MODBUS =      spec(0x8005, 0xffff, true,  true,  0x0000, 0x4b37)
CRC_16_X_25 =        spec(0x1021, 0xffff, true,  true,  0xffff, 0x906e)
CRC_16_XMODEM =      spec(0x1021, 0x0000, false, false, 0x0000, 0x31c3)

# http://reveng.sourceforge.net/crc-catalogue/17plus.htm
CRC_24 =             spec(24, 0x00864cfb, 0x00b704ce, false, false, 0x00000000, 0x0021cf02)
CRC_24_FLEXRAY_A =   spec(24, 0x005d6dcb, 0x00fedcba, false, false, 0x00000000, 0x007979bd)
CRC_24_FLEXRAY_B =   spec(24, 0x005d6dcb, 0x00abcdef, false, false, 0x00000000, 0x001f23b8)
CRC_31_PHILIPS =     spec(31, 0x04c11db7, 0x7fffffff, false, false, 0x7fffffff, 0x0ce9e46c)
CRC_32 =             spec(0x04c11db7, 0xffffffff, true,  true,  0xffffffff, 0xcbf43926)
CRC_32_BZIP2 =       spec(0x04c11db7, 0xffffffff, false, false, 0xffffffff, 0xfc891918)
CRC_32_C =           spec(0x1edc6f41, 0xffffffff, true,  true,  0xffffffff, 0xe3069283)
CRC_32_D =           spec(0xa833982b, 0xffffffff, true,  true,  0xffffffff, 0x87315576)
CRC_32_MPEG_2 =      spec(0x04c11db7, 0xffffffff, false, false, 0x00000000, 0x0376e6e7)
CRC_32_POSIX =       spec(0x04c11db7, 0x00000000, false, false, 0xffffffff, 0x765e7680)
CRC_32_Q =           spec(0x814141ab, 0x00000000, false, false, 0x00000000, 0x3010bf7f)
CRC_32_JAMCRC =      spec(0x04c11db7, 0xffffffff, true,  true,  0x00000000, 0x340bc6d9)
CRC_32_XFER =        spec(0x000000af, 0x00000000, false, false, 0x00000000, 0xbd0be338)
CRC_40_GSM =         spec(40, 0x0000000004820009, 0x0000000000000000, false, false, 0x000000ffffffffff, 0x000000d4164fc646)
CRC_64 =             spec(0x42f0e1eba9ea3693, 0x0000000000000000, false, false, 0x0000000000000000, 0x6c40df5f0b497347)
CRC_64_WE =          spec(0x42f0e1eba9ea3693, 0xffffffffffffffff, false, false, 0xffffffffffffffff, 0x62ec59e3f1a4f00a)
CRC_64_XZ =          spec(0x42f0e1eba9ea3693, 0xffffffffffffffff, true,  true,  0xffffffffffffffff, 0x995dc9bbdf1939fa)
CRC_82_DARC =        spec(82, 0x0308c0111011401440411, 0x000000000000000000000, true,  true,   0x000000000000000000000, 0x09ea83f625023801fd612)

const ALL = Dict{Symbol,Spec}(:CRC_3_ROHC=>CRC_3_ROHC,
                            :CRC_4_ITU=>CRC_4_ITU,
                            :CRC_5_EPC=>CRC_5_EPC,
                            :CRC_5_ITU=>CRC_5_ITU,
                            :CRC_5_USB=>CRC_5_USB,
                            :CRC_6_CDMA2000_A=>CRC_6_CDMA2000_A,
                            :CRC_6_CDMA2000_B=>CRC_6_CDMA2000_B,
                            :CRC_6_DARC=>CRC_6_DARC,
                            :CRC_6_ITU=>CRC_6_ITU, :CRC_7=>CRC_7,
                            :CRC_7_ROHC=>CRC_7_ROHC, :CRC_8=>CRC_8,
                            :CRC_8_CDMA2000=>CRC_8_CDMA2000,
                            :CRC_8_DARC=>CRC_8_DARC,
                            :CRC_8_DVB_S2=>CRC_8_DVB_S2,
                            :CRC_8_EBU=>CRC_8_EBU,
                            :CRC_8_I_CODE=>CRC_8_I_CODE,
                            :CRC_8_ITU=>CRC_8_ITU,
                            :CRC_8_MAXIM=>CRC_8_MAXIM,
                            :CRC_8_ROHC=>CRC_8_ROHC,
                            :CRC_8_WCDMA=>CRC_8_WCDMA,
                            :CRC_10=>CRC_10,
                            :CRC_10_CDMA2000=>CRC_10_CDMA2000,
                            :CRC_11=>CRC_11,
                            :CRC_12_3GPP=>CRC_12_3GPP,
                            :CRC_12_CDMA2000=>CRC_12_CDMA2000,
                            :CRC_12_DECT=>CRC_12_DECT,
                            :CRC_13_BBC=>CRC_13_BBC,
                            :CRC_14_DARC=>CRC_14_DARC,
                            :CRC_15=>CRC_15,
                            :CRC_15_MPT1327=>CRC_15_MPT1327,
                            :CRC_16_ARC=>CRC_16_ARC,
                            :CRC_16_AUG_CCITT=>CRC_16_AUG_CCITT,
                            :CRC_16_BUYPASS=>CRC_16_BUYPASS,
                            :CRC_16_CCITT_FALSE=>CRC_16_CCITT_FALSE,
                            :CRC_16_CDMA2000=>CRC_16_CDMA2000,
                            :CRC_16_DDS_110=>CRC_16_DDS_110,
                            :CRC_16_DECT_R=>CRC_16_DECT_R,
                            :CRC_16_DECT_X=>CRC_16_DECT_X,
                            :CRC_16_DNP=>CRC_16_DNP,
                            :CRC_16_EN_13757=>CRC_16_EN_13757,
                            :CRC_16_GENIBUS=>CRC_16_GENIBUS,
                            :CRC_16_MAXIM=>CRC_16_MAXIM,
                            :CRC_16_RIELLO=>CRC_16_RIELLO,
                            :CRC_16_TELEDISK=>CRC_16_TELEDISK,
                            :CRC_16_USB=>CRC_16_USB,
                            :CRC_16_CRC_A=>CRC_16_CRC_A,
                            :CRC_16_KERMIT=>CRC_16_KERMIT,
                            :CRC_16_MODBUS=>CRC_16_MODBUS,
                            :CRC_16_X_25=>CRC_16_X_25,
                            :CRC_16_XMODEM=>CRC_16_XMODEM,
                            :CRC_24=>CRC_24,
                            :CRC_24_FLEXRAY_A=>CRC_24_FLEXRAY_A,
                            :CRC_24_FLEXRAY_B=>CRC_24_FLEXRAY_B,
                            :CRC_31_PHILIPS=>CRC_31_PHILIPS,
                            :CRC_32=>CRC_32,
                            :CRC_32_BZIP2=>CRC_32_BZIP2,
                            :CRC_32_C=>CRC_32_C, :CRC_32_D=>CRC_32_D,
                            :CRC_32_MPEG_2=>CRC_32_MPEG_2,
                            :CRC_32_POSIX=>CRC_32_POSIX,
                            :CRC_32_Q=>CRC_32_Q,
                            :CRC_32_JAMCRC=>CRC_32_JAMCRC,
                            :CRC_32_XFER=>CRC_32_XFER,
                            :CRC_40_GSM=>CRC_40_GSM, :CRC_64=>CRC_64,
                            :CRC_64_WE=>CRC_64_WE,
                            :CRC_64_XZ=>CRC_64_XZ,
                            :CRC_82_DARC=>CRC_82_DARC)



# --- Utilities


# http://stackoverflow.com/questions/2602823/in-c-c-whats-the-simplest-way-to-reverse-the-order-of-bits-in-a-byte
function reflect_bits(n::UInt8)
    n = (n & 0xf0) >>> 4 | (n & 0x0f) << 4;
    n = (n & 0xcc) >>> 2 | (n & 0x33) << 2;
    (n & 0xaa) >>> 1 | (n & 0x55) << 1;
end

const REFLECT_8 = UInt8[reflect_bits(i) for i in 0x00:0xff]

reflect(u::UInt8) = REFLECT_8[u+1]

for (T,S) in ((UInt16, UInt8), (UInt32, UInt16), (UInt64, UInt32), (UInt128, UInt64))
    n = 8 * sizeof(S)
    mask::S = -one(S)
    @eval reflect(u::$T) = (convert($T, reflect(convert($S, u & $mask))) << $n) | reflect(convert($S, (u >>> $n) & $mask))
end

function reflect(size, u::T) where {T<:U}
    width = 8 * sizeof(T)
    @assert size <= width "cannot reflect a value larger than the representation"
    reflect(u) >>> (width - size)
end


function largest(T, TS...)
    for t in TS
        if sizeof(t) > sizeof(T)
            T = t
        end
    end
    T
end

function fastest(T, TS...)
    L = largest(T, TS...)
    if L == UInt32 && UInt32 != UInt
        UInt64  # round up for speed
    else
        L
    end
end


function pad(::Type{A}, width) where {A<:U}
    8 * sizeof(A) - width
end



# --- CRC calculations


# a calculation may be a "natural" long division (padded to the size
# of the accumulator) or "reverse the rest of the world".  this
# depends on whether the input input bytes are taken as given
# (Forwards) or reflected (Backwards).

abstract type Direction{A<:U} end

struct Backwards{A<:U}<:Direction{A}
    poly::A
    init::A
    function (::Type{Backwards{A}})(spec::Spec) where {A}
        poly = reflect(spec.width, convert(A, spec.poly))
        init = reflect(spec.width, convert(A, spec.init))
        new{A}(poly, init)
    end
end

struct Forwards{A<:U}<:Direction{A}
    pad_p::Int
    poly::A
    carry::A
    pad_8::Int
    init::A
    function (::Type{Forwards{A}})(spec::Spec) where {A}
        pad_p = pad(A, spec.width)
        poly = convert(A, spec.poly) << pad_p
        carry = one(A) << pad(A, 1)
        pad_8 = pad(A, 8)
        init = convert(A, spec.init) << pad_p
        new{A}(pad_p, poly, carry, pad_8, init)
    end
end


# a calculation may use no, one, or many tables...

abstract type Tables{A<:U} end

struct NoTables{A<:U}<:Tables{A}
    (::Type{NoTables{A}})(direcn::Direction{A}) where {A} = new{A}()
end

struct Multiple{A<:U}<:Tables{A}
    tables::Vector{Vector{A}}
    function (::Type{Multiple{A}})(direcn::Direction{A}) where {A}
        tables = Vector{A}[Array{A}(undef, 256) for _ in 1:sizeof(A)]
        fill_table(direcn, tables[1])
        for index in zero(UInt8):convert(UInt8, 255)
            remainder = tables[1][index + 1]
            for t in 2:sizeof(A)
                # chaining gives the contribution of a byte to the
                # remainder after being shifted through t other zero bytes
                # (the different bytes are then combined with xor).
                remainder = chain(direcn, tables[1], remainder)
                tables[t][index + 1] = remainder
            end
        end
        new{A}(tables)
    end
end

struct Single{A<:U}<:Tables{A}
    table::Vector{A}
    # when using multiple tables we need a related single table for the
    # "last few bytes"
    (::Type{Single{A}})(tables::Multiple{A}) where {A} = new{A}(tables.tables[1])
    (::Type{Single{A}})(direcn::Direction{A}) where {A} = new{A}(fill_table(direcn, Array{A}(undef, 256)))
end

# the basic lookup table is just the remainder for each value
function fill_table(direcn, table::Vector{A}) where {A<:U}
    for index in zero(UInt8):convert(UInt8, 255)
        table[index + 1] = extend(direcn, NoTables{A}(direcn), [index], zero(A))
    end
    table
end

function chain(direcn::Forwards{A}, table::Vector{A}, remainder::A) where {A<:U}
    xor(remainder << 8, table[1 + ((remainder >>> direcn.pad_8) & 0xff)])
end

function chain(direcn::Backwards{A}, table::Vector{A}, remainder::A) where {A<:U}
    xor(remainder >>> 8, table[1 + (remainder & 0xff)])
end

"""
    crc(spec[, tables = Multiple])

Create a crc function implementing `spec` with lookup table behaviour `tables`.
Available specs are listed in [`ALL`](@ref) and tables can be either
[`Multiple`](@ref), [`Single`](@ref) or [`NoTables`](@ref).

The returned function accepts `String`, `Vector{UInt8}` or
`IO` and calculates the CRC checksum. An optional boolean parameter
`append` can be used for chained application.

A faster CRC32*C* implementation is available in Stdlib: [`CRC32c`](@ref).

# Examples
```julia-repl
using CRC

# create our own crc function, just once
crc32 = crc(CRC_32)

# use the crc function created above, many times
for s in ["hello", "there"]
    println(s => crc32(s))
end
```
"""
function crc(spec::Spec{P}; tables::Type{T}=Multiple) where {P<:U, T<:Tables}
    A = fastest(P, tables == Multiple ? UInt : UInt8)
    direcn = spec.refin ? Backwards{A}(spec) : Forwards{A}(spec)
    remainder::A = direcn.init
    tables = tables{A}(direcn)
    function handler(data::Vector{UInt8}; append=false)
        remainder = append ? remainder : direcn.init
        remainder = extend(direcn, tables, data, remainder)
        finalize(spec, direcn, remainder)
    end
    function handler(io::IO; append=false, buflen=1000000)
        buffer = Array{UInt8}(undef, buflen)
        remainder = append ? remainder : direcn.init
        while (nb = readbytes!(io, buffer)) > 0
            remainder = extend(direcn, tables, buffer[1:nb], remainder)
        end
        finalize(spec, direcn, remainder)
    end
    function handler(data::AbstractString; append=false)
        handler(convert(Vector{UInt8}, codeunits(data)), append=append)
    end
    handler
end


# calculations assume that the remainder is either reflected or
# padded; these functions correct for this to return the final result.

function finalize(spec::Spec{P}, direcn::Forwards{A}, remainder::A) where {P<:U, A<:U}
    remainder = convert(P, remainder >> direcn.pad_p)
    remainder = spec.refout ? reflect(spec.width, remainder) : remainder
    xor(remainder, spec.xorout)
end

function finalize(spec::Spec{P}, direcn::Backwards{A}, remainder::A) where {P<:U, A<:U}
    remainder = spec.refout ? remainder : reflect(spec.width, remainder)
    xor(convert(P, remainder), spec.xorout)
end


# all the routines below calculate a new value of remainder for the
# data given (along with the algorithm type, available lookup tables,
# etc).

const UNROLL = 16  # only get small improvements past this

function extend(direcn::Forwards{A}, tables::NoTables, data::Vector{UInt8}, remainder::A) where {A<:U}
    for word::UInt8 in data
        remainder::A = xor(remainder, (convert(A, word) << direcn.pad_8))
        for _ in 1:8
            if remainder & direcn.carry == direcn.carry
                remainder = xor(remainder << 1, direcn.poly)
            else
                remainder <<= 1
            end
        end
    end
    remainder
end

function extend(direcn::Forwards{A}, tables::Single{A}, data::Vector{UInt8}, remainder::A) where {A<:U}
    for i in 1:length(data)
        @inbounds word::UInt8 = data[i]
        remainder::A = xor(remainder, convert(A, word) << direcn.pad_8)
        remainder = xor(remainder << 8, tables.table[1 + remainder >>> direcn.pad_8])
    end
    remainder
end

for A in (UInt16, UInt32, UInt64, UInt128)
    n_tables = sizeof(A)
    @eval begin
        function extend(direcn::Forwards{$A}, tables::Multiple{$A}, data::MaybeUnalignedVector{$A}, remainder::$A)
            word::$A, tmp::$A, remainder::$A = zero($A), zero($A), remainder
            i = 1
            while true
                @nexprs $UNROLL _ -> begin  # unroll inner loop
                    if i > length(data)
                        break
                    end
                    @inbounds word = data[i]
                    tmp, remainder = remainder, zero($A)
                    @nexprs $n_tables t -> begin  # unroll table
                        # access similar to reflected but we need to
                        # handle little-endian bytes which turn out
                        # wrong for this case
                        remainder = xor(remainder,
                          tables.tables[t][xor(word >>> (($n_tables - t)*8), tmp >>> ((t-1)*8)) & 0xff + 1])
                    end
                    i += 1
                end
            end
            remainder
        end
    end
end

function extend(direcn::Backwards{A}, tables::NoTables, data::Vector{UInt8}, remainder::A) where {A<:U}
    for word::UInt8 in data
        remainder::A = xor(remainder, convert(A, word))
        for _ in 1:8
            if remainder & one(A) == one(A)
                remainder = xor(remainder >>> 1, direcn.poly)
            else
                remainder >>>= 1
            end
        end
    end
    remainder
end

function extend(direcn::Backwards{A}, tables::Single{A}, data::Vector{UInt8}, remainder::A) where {A<:U}
    for i in 1:length(data)
        @inbounds word::UInt8 = data[i]
        remainder::A = xor(remainder, convert(A, word))
        remainder = xor(remainder >>> 8, tables.table[1 + remainder & 0xff])
    end
    remainder
end

# generate code for processing a bunch of bytes in a single machine
# word, using multiple tables.  stolen from libz.
for A in (UInt16, UInt32, UInt64, UInt128)
    n_tables = sizeof(A)
    @eval begin
        function extend(direcn::Backwards{$A}, tables::Multiple{$A}, data::MaybeUnalignedVector{$A}, remainder::$A)
            word::$A, tmp::$A, remainder::$A = zero($A), zero($A), remainder
            i = 1
            while true
                @nexprs $UNROLL _ -> begin  # unroll inner loop
                    if i > length(data)
                        break
                    end
                    @inbounds word = data[i]
                    tmp, remainder = xor(remainder, word), zero($A)
                    @nexprs $n_tables t -> begin  # unroll table access
                        remainder = xor(remainder,
                          tables.tables[t][(tmp >>> (($n_tables - t)*8)) & 0xff + 1])
                    end
                    i += 1
                end
            end
            remainder
        end
    end
end

# short-circuit "all UInt8" avoiding a self-recursive loop below
function extend(direcn::Direction{UInt8}, tables::Multiple{UInt8}, data::Vector{UInt8}, remainder::UInt8)
    extend(direcn, Single{UInt8}(tables), data, remainder)
end

function extend(direcn::Direction{A}, tables::Multiple{A}, data::Vector{UInt8}, remainder::A) where {A<:U}
    # this is "clever" - we alias the array of bytes to native machine
    # words and then process those, which typically (64 bits) loads 8
    # bytes at a time.
    tail = length(data) % sizeof(A)
    remainder = extend(direcn, tables, unaligned_reinterpret(A, data), remainder)
    # slurp up the final bytes that didn't fill a complete machine word.
    extend(direcn, Single{A}(tables), data[end-tail+1:end], remainder)
end
