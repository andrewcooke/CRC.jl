
# calculate crc checksums for data.  this process is closely related
# to the routines in IntModN (particularly GF2Poly), but the
# traditional implementations are optimized to the point where
# implementing them from "first principles" makes little sense
# (although we can use IntModN to check the results here).

# the basic idea is to do a polynomial division along the byte stream.
# it's all explained very nicely at
# http://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
# and http://www.zlib.net/crc_v3.txt


module CRC

export crc, crc_new, make_tables, TEST, CRC_3_ROHC, CRC_4_ITU, CRC_5_EPC,
       CRC_5_ITU, CRC_5_USB, CRC_6_CDMA2000_A, CRC_6_CDMA2000_B,
       CRC_6_DARC, CRC_6_ITU, CRC_7, CRC_7_ROHC, CRC_8,
       CRC_8_CDMA2000, CRC_8_DARC, CRC_8_DVB_S2, CRC_8_EBU,
       CRC_8_I_CODE, CRC_8_ITU, CRC_8_MAXIM, CRC_8_ROHC, CRC_8_WCDMA,
       CRC_10, CRC_10_CDMA2000, CRC_11, CRC_12_3GPP, CRC_12_CDMA2000,
       CRC_12_DECT, CRC_13_BBC, CRC_14_DARC, CRC_15, CRC_15_MPT1327,
       CRC_16_ARC, CRC_16_AUG_CCITT, CRC_16_BUYPASS,
       CRC_16_CCITT_FALSE, CRC_16_CDMA2000, CRC_16_DDS_110,
       CRC_16_DECT_R, CRC_16_DECT_X, CRC_16_DNP, CRC_16_EN_13757,
       CRC_16_GENIBUS, CRC_16_MAXIM, CRC_16_RIELLO, CRC_16_TELEDISK,
       CRC_16_USB, CRC_16_CRC_A, CRC_16_KERMIT, CRC_16_MODBUS,
       CRC_16_X_25, CRC_16_XMODEM, CRC_24, CRC_24_FLEXRAY_A,
       CRC_24_FLEXRAY_B, CRC_31_PHILIPS, CRC_32, CRC_32_BZIP2,
       CRC_32_C, CRC_32_D, CRC_32_MPEG_2, CRC_32_POSIX, CRC_32_Q,
       CRC_32_JAMCRC, CRC_32_XFER, CRC_40_GSM, CRC_64, CRC_64_WE,
       CRC_64_XZ, CRC_82_DARC

typealias U Unsigned

TEST = b"123456789"

# TODO - assert type has space for width etc
type Spec{P<:U}
    # http://www.zlib.net/crc_v3.txt
    width::Int    # polynomial degree
    poly::P       # generating poly with msb missing
    init::P       # initial remainder
    refin::Bool   # reflect input
    refout::Bool  # reflect output
    xorout::P     # xored with final remainder
    test::P       # checksum for TEST
end

Spec{P<:U}(poly::P, init::P, refin::Bool, refout::Bool, xorout::P, test::P) = 
    Spec(8*sizeof(P), poly, init, refin, refout, xorout, test)


# http://reveng.sourceforge.net/crc-catalogue/1-15.htm
CRC_3_ROHC =         Spec(3, 0x03, 0x07, true,  true,  0x00, 0x06)
CRC_4_ITU =          Spec(4, 0x03, 0x00, true,  true,  0x00, 0x07)
CRC_5_EPC =          Spec(5, 0x09, 0x09, false, false, 0x00, 0x00)
CRC_5_ITU =          Spec(5, 0x15, 0x00, true,  true,  0x00, 0x07)
CRC_5_USB =          Spec(5, 0x05, 0x1f, true,  true,  0x1f, 0x19)
CRC_6_CDMA2000_A =   Spec(6, 0x27, 0x3f, false, false, 0x00, 0x0d)
CRC_6_CDMA2000_B =   Spec(6, 0x07, 0x3f, false, false, 0x00, 0x3b)
CRC_6_DARC =         Spec(6, 0x19, 0x00, true,  true,  0x00, 0x26)
CRC_6_ITU =          Spec(6, 0x03, 0x00, true,  true,  0x00, 0x06)
CRC_7 =              Spec(7, 0x09, 0x00, false, false, 0x00, 0x75)
CRC_7_ROHC =         Spec(7, 0x4f, 0x7f, true,  true,  0x00, 0x53)
CRC_8 =              Spec(0x07, 0x00, false, false, 0x00, 0xf4)
CRC_8_CDMA2000 =     Spec(0x9b, 0xff, false, false, 0x00, 0xda)
CRC_8_DARC =         Spec(0x39, 0x00, true,  true,  0x00, 0x15)
CRC_8_DVB_S2 =       Spec(0xd5, 0x00, false, false, 0x00, 0xbc)
CRC_8_EBU =          Spec(0x1d, 0xff, true,  true,  0x00, 0x97)
CRC_8_I_CODE =       Spec(0x1d, 0xfd, false, false, 0x00, 0x7e)
CRC_8_ITU =          Spec(0x07, 0x00, false, false, 0x55, 0xa1)
CRC_8_MAXIM =        Spec(0x31, 0x00, true,  true,  0x00, 0xa1)
CRC_8_ROHC =         Spec(0x07, 0xff, true,  true,  0x00, 0xd0)
CRC_8_WCDMA =        Spec(0x9b, 0x00, true,  true,  0x00, 0x25)
CRC_10 =             Spec(10, 0x0233, 0x0000, false, false, 0x0000, 0x0199)
CRC_10_CDMA2000 =    Spec(10, 0x03d9, 0x03ff, false, false, 0x0000, 0x0233)
CRC_11 =             Spec(11, 0x0385, 0x001a, false, false, 0x0000, 0x05a3)
CRC_12_3GPP =        Spec(12, 0x080f, 0x0000, false, true,  0x0000, 0x0daf)
CRC_12_CDMA2000 =    Spec(12, 0x0f13, 0x0fff, false, false, 0x0000, 0x0d4d)
CRC_12_DECT =        Spec(12, 0x080f, 0x0000, false, false, 0x0000, 0x0f5b)
CRC_13_BBC =         Spec(13, 0x1cf5, 0x0000, false, false, 0x0000, 0x04fa)
CRC_14_DARC =        Spec(14, 0x0805, 0x0000, true,  true,  0x0000, 0x082d)
CRC_15 =             Spec(15, 0x4599, 0x0000, false, false, 0x0000, 0x059e)
CRC_15_MPT1327 =     Spec(15, 0x6815, 0x0000, false, false, 0x0001, 0x2566)

# http://reveng.sourceforge.net/crc-catalogue/16.htm#crc.cat.crc-16-ccitt-false
CRC_16_ARC =         Spec(0x8005, 0x0000, true,  true,  0x0000, 0xbb3d)
CRC_16_AUG_CCITT =   Spec(0x1021, 0x1d0f, false, false, 0x0000, 0xe5cc)
CRC_16_BUYPASS   =   Spec(0x8005, 0x0000, false, false, 0x0000, 0xfee8)
CRC_16_CCITT_FALSE = Spec(0x1021, 0xffff, false, false, 0x0000, 0x29b1)
CRC_16_CDMA2000 =    Spec(0xc867, 0xffff, false, false, 0x0000, 0x4c06)
CRC_16_DDS_110 =     Spec(0x8005, 0x800d, false, false, 0x0000, 0x9ecf)
CRC_16_DECT_R =      Spec(0x0589, 0x0000, false, false, 0x0001, 0x007e)
CRC_16_DECT_X =      Spec(0x0589, 0x0000, false, false, 0x0000, 0x007f)
CRC_16_DNP =         Spec(0x3d65, 0x0000, true,  true,  0xffff, 0xea82)
CRC_16_EN_13757 =    Spec(0x3d65, 0x0000, false, false, 0xffff, 0xc2b7)
CRC_16_GENIBUS =     Spec(0x1021, 0xffff, false, false, 0xffff, 0xd64e)
CRC_16_MAXIM =       Spec(0x8005, 0x0000, true,  true,  0xffff, 0x44c2)
CRC_16_RIELLO =      Spec(0x8bb7, 0x0000, false, false, 0x0000, 0xd0db)
CRC_16_TELEDISK =    Spec(0x1021, 0x89ec, true,  true,  0x0000, 0x26b1)
CRC_16_USB =         Spec(0x8005, 0xffff, true,  true,  0xffff, 0xb4c8)
CRC_16_CRC_A =       Spec(0x1021, 0xc6c6, true,  true,  0x0000, 0xbf05)
CRC_16_KERMIT =      Spec(0x1021, 0x0000, true,  true,  0x0000, 0x2189)
CRC_16_MODBUS =      Spec(0x8005, 0xffff, true,  true,  0x0000, 0x4b37)
CRC_16_X_25 =        Spec(0x1021, 0xffff, true,  true,  0xffff, 0x906e)
CRC_16_XMODEM =      Spec(0x1021, 0x0000, false, false, 0x0000, 0x31c3)

# http://reveng.sourceforge.net/crc-catalogue/17plus.htm
CRC_24 =             Spec(24, 0x00864cfb, 0x00b704ce, false, false, 0x00000000, 0x0021cf02)
CRC_24_FLEXRAY_A =   Spec(24, 0x005d6dcb, 0x00fedcba, false, false, 0x00000000, 0x007979bd)
CRC_24_FLEXRAY_B =   Spec(24, 0x005d6dcb, 0x00abcdef, false, false, 0x00000000, 0x001f23b8)
CRC_31_PHILIPS =     Spec(31, 0x04c11db7, 0x7fffffff, false, false, 0x7fffffff, 0x0ce9e46c)
CRC_32 =             Spec(0x04c11db7, 0xffffffff, true,  true,  0xffffffff, 0xcbf43926)
CRC_32_BZIP2 =       Spec(0x04c11db7, 0xffffffff, false, false, 0xffffffff, 0xfc891918)
CRC_32_C =           Spec(0x1edc6f41, 0xffffffff, true,  true,  0xffffffff, 0xe3069283)
CRC_32_D =           Spec(0xa833982b, 0xffffffff, true,  true,  0xffffffff, 0x87315576)
CRC_32_MPEG_2 =      Spec(0x04c11db7, 0xffffffff, false, false, 0x00000000, 0x0376e6e7)
CRC_32_POSIX =       Spec(0x04c11db7, 0x00000000, false, false, 0xffffffff, 0x765e7680)
CRC_32_Q =           Spec(0x814141ab, 0x00000000, false, false, 0x00000000, 0x3010bf7f)
CRC_32_JAMCRC =      Spec(0x04c11db7, 0xffffffff, true,  true,  0x00000000, 0x340bc6d9)
CRC_32_XFER =        Spec(0x000000af, 0x00000000, false, false, 0x00000000, 0xbd0be338)
CRC_40_GSM =         Spec(40, 0x0000000004820009, 0x0000000000000000, false, false, 0x000000ffffffffff, 0x000000d4164fc646)
CRC_64 =             Spec(0x42f0e1eba9ea3693, 0x0000000000000000, false, false, 0x0000000000000000, 0x6c40df5f0b497347)
CRC_64_WE =          Spec(0x42f0e1eba9ea3693, 0xffffffffffffffff, false, false, 0xffffffffffffffff, 0x62ec59e3f1a4f00a)
CRC_64_XZ =          Spec(0x42f0e1eba9ea3693, 0xffffffffffffffff, true,  true,  0xffffffffffffffff, 0x995dc9bbdf1939fa)
CRC_82_DARC =        Spec(82, 0x0308c0111011401440411, 0x000000000000000000000, true,  true,   0x000000000000000000000, 0x09ea83f625023801fd612)


# http://stackoverflow.com/questions/2602823/in-c-c-whats-the-simplest-way-to-reverse-the-order-of-bits-in-a-byte
function reflect_bits(n::Uint8)
    n = (n & 0xf0) >>> 4 | (n & 0x0f) << 4;
    n = (n & 0xcc) >>> 2 | (n & 0x33) << 2;
    (n & 0xaa) >>> 1 | (n & 0x55) << 1;
end

REFLECT_8 = Uint8[reflect_bits(i) for i in 0x00:0xff]

reflect(u::Uint8) = REFLECT_8[u+1]

for (T,S) in ((Uint16, Uint8), (Uint32, Uint16), (Uint64, Uint32), (Uint128, Uint64))
    n = 8 * sizeof(S)
    mask::S = -one(S)
    @eval reflect(u::$T) = (convert($T, reflect(convert($S, u & $mask))) << $n) | reflect(convert($S, (u >>> $n) & $mask))
end

function reflect{T<:U}(size, u::T)
    width = 8 * sizeof(T)
    @assert size <= width "cannot reflect a value larger than the representation"
    reflect(u) >>> (width - size)
end

function itype(iterable, default)
    for i in iterable
        return typeof(i)
    end
    return default
end

function to_uint(size_or_type)
    if isa(size_or_type, Type) && issubtype(size_or_type, U) && isleaftype(size_or_type)
        return size_or_type
    elseif isa(size_or_type, Integer) && size_or_type > 0
        if size_or_type <= 8
            return Uint8
        elseif size_or_type <= 16
            return Uint16
        elseif size_or_type <= 32
            return Uint32
        elseif size_or_type <= 64
            return Uint64
        elseif size_or_type <= 128
            return Uint128
        end
    end
    error("unexpected type / size: $size_or_type ($(typeof(size_or_type)))")
end

function largest(T, TS...)
    big = to_uint(T)
    for t in map(to_uint, TS)
        if sizeof(t) > sizeof(big)
            big = t
        end
    end
    big
end

function fastest(T, TS...)
    l = largest(T, TS...)
    if l == Uint32 && Uint32 != Uint
        Uint64  # round up for speed
    else
        l
    end
end

function pad{A<:U}(::Type{A}, width)
    8 * sizeof(A) - width
end

function pad{A<:U, D<:U}(::Type{A}, ::Type{D})
    pad(A, 8*sizeof(D))
end

INDEX_SIZE = 8
TABLE_SIZE = 256

function make_tables{D<:U, A<:U, P<:U
                     }(::Type{D}, ::Type{A}, width, poly::P, refin)

    n_tables = 8 * sizeof(D) / INDEX_SIZE
    tables = Vector{A}[Array(A, TABLE_SIZE) for _ in 1:n_tables]

    if refin
        make_tables_ref(tables, D, width, poly)
    else
        make_tables_pad(tables, D, width, poly)
    end
end

function make_tables_ref{D<:U, A<:U, P<:U
                         }(tables::Vector{Vector{A}}, ::Type{D}, width, poly::P)

    poly = reflect(width, poly)

    for index in zero(Uint8):convert(Uint8, TABLE_SIZE-1)
        remainder::A = convert(A, index)
        for _ in 1:INDEX_SIZE
            if remainder & one(A) == one(A)
                remainder = (remainder >>> 1) $ poly
            else
               remainder >>>= 1
            end
        end
        tables[1][index + 1] = remainder
    end
    for index in zero(Uint8):convert(Uint8, TABLE_SIZE-1)
        remainder = tables[1][index + 1]
        for t in 2:length(tables)
            remainder = (remainder >>> INDEX_SIZE) $ tables[1][1 + (remainder & 0xff)]
            tables[t][index + 1] = remainder
        end
    end
    tables
end

function make_tables_pad{D<:U, A<:U, P<:U
                         }(tables::Vector{Vector{A}}, ::Type{D}, width, poly::P)

    pad_p = pad(A, width)
    poly = convert(A, poly) << pad_p
    carry = one(A) << pad(A, 1)
    pad_8 = pad(A, 8)

    for index in zero(Uint8):convert(Uint8, TABLE_SIZE-1)
        remainder::A = convert(A, index) << pad_8
        for _ in 1:INDEX_SIZE
            if remainder & carry == carry
                remainder = (remainder << 1) $ poly
            else
                remainder <<= 1
            end
        end
        tables[1][index + 1] = remainder
    end
    for index in zero(Uint8):convert(Uint8, TABLE_SIZE-1)
        remainder = tables[1][index + 1]
        for t in 2:length(tables)
            remainder = (remainder << INDEX_SIZE) $ tables[1][1 + ((remainder >>> pad_8) & 0xff)]
            tables[t][index + 1] = remainder
        end
    end
    tables
end

function extend{P<:U, A<:U}(poly::P, width, refin, remainder::P, data, tables::Vector{Vector{A}})

    D = itype(data, Uint8)
    @assert largest(D, P, A) == A

    pad_p = refin ? 0 : pad(A, width)
    poly = refin ? reflect(width, poly) : poly
    poly = convert(A, poly) << pad_p
    remainder = refin ? reflect(width, remainder) : remainder
    remainder = convert(A, remainder) << pad_p

    if length(tables) == 0
        if refin
            remainder = loop_no_tables_ref(D, poly, remainder, data)
        else
            remainder = loop_no_tables_pad(D, poly, remainder, data)
        end
    else
        if refin
            remainder = loop_tables_ref(D, poly, remainder, data, tables)
        else
            remainder = loop_tables_pad(D, poly, remainder, data, tables)
        end
    end
    
    remainder >>>= pad_p
    remainder = refin ? reflect(width, remainder) : remainder
    convert(P, remainder)
end

function loop_no_tables_ref{D<:U, A<:U
                            }(::Type{D}, poly::A, remainder::A, data)
    for word::D in data
        remainder::A = remainder $ convert(A, word)
        for _ in 1:8*sizeof(D)
            if remainder & one(A) == one(A)
                remainder = (remainder >>> 1) $ poly
            else
                remainder >>>= 1
            end
        end
    end
    remainder
end

function loop_no_tables_pad{D<:U, A<:U
                            }(::Type{D}, poly::A, remainder::A, data)

    pad_d = pad(A, D)
    carry = one(A) << pad(A, 1)

    for word::D in data
        remainder::A = remainder $ (convert(A, word) << pad_d)
        for _ in 1:8*sizeof(D)
            if remainder & carry == carry
                remainder = (remainder << 1) $ poly
            else
                remainder <<= 1
            end
        end
    end
    remainder
end

function loop_tables_ref{D<:U, A<:U
                         }(::Type{D}, poly::A, remainder::A, data, tables::Vector{Vector{A}})

    n_tables = length(tables)

    for word::D in data
        tmp::A = remainder $ convert(A, word)
        remainder = tmp >>> 8*sizeof(D)
        # TODO - unroll statically
        for t in 1:n_tables
            remainder $= tables[t][(tmp >>> (n_tables - t)*8) & 0xff + 1]
        end
    end
    remainder
end

function loop_tables_pad{D<:U, A<:U
                         }(::Type{D}, poly::A, remainder::A, data, tables::Vector{Vector{A}})

    pad_d = pad(A, D)
    pad_8 = pad(A, 8)
    n_tables = length(tables)

    for word::D in data
        tmp::A = remainder $ (convert(A, word) << pad_d)
        remainder = tmp << 8*sizeof(D)
        # TODO - unroll statically
        for t in 1:n_tables
            remainder $= tables[t][(tmp >>> (pad_8 - (t-1)*8)) & 0xff + 1]
        end        
    end
    remainder
end

function finalize_remainder(remainder, width, refout, xorout)
    remainder = refout ? reflect(width, remainder) : remainder
    remainder $ xorout
end


function crc{P<:U}(spec::Spec{P}, data)
    A = fastest(P, itype(data, Uint8))
    finalize_remainder(extend(spec.poly, spec.width, spec.refin, spec.init, data, Vector{A}[]), spec.width, spec.refout, spec.xorout)
end

# flag switches implementations (true - new multi)
function crc{P<:U, D<:U}(spec::Spec{P}, ::Type{D}, flag)
    A = (D == Uint8 && flag) ? fastest(P, D, Uint) : fastest(P, D)
    tables_d = make_tables(D, A, spec.width, spec.poly, spec.refin)
    function crc_local(data)
        finalize_remainder(extend(spec.poly, spec.width, spec.refin, spec.init, data, tables_d), spec.width, spec.refout, spec.xorout)
    end
    if D == Uint8 && flag
        tables_uint = make_tables(Uint, A, spec.width, spec.poly, spec.refin)
        function crc_local(data::Vector{D})
            tail = length(data) % div(sizeof(Uint), sizeof(D))
            remainder = extend(spec.poly, spec.width, spec.refin, spec.init, reinterpret(Uint, data), tables_uint)
            remainder = extend(spec.poly, spec.width, spec.refin, remainder, data[end-tail+1:end], tables_d)
            finalize_remainder(remainder, spec.width, spec.refout, spec.xorout)
        end
    end
    crc_local
end


abstract Table

immutable NoTable<:Table end

type Multiple{A<:U}<:Table 
    tables::Vector{Vector{A}}
    Multiple() = new()
end

type Single{A<:U}<:Table 
    table::Vector{A}
    Single() = new()
end

function Single{A<:U}(tables::Multiple{A})
    table = Single{A}()
    table.table = tables.tables[1]
    table
end

abstract Algorithm{A<:U}

immutable Reversed{A<:U}<:Algorithm{A}
    poly::A
    init::A
end

function Reversed{P<:U}(spec::Spec{P})
    A = fastest(P, Uint8)
    poly = reflect(spec.width, convert(A, spec.poly))
    init = reflect(spec.width, convert(A, spec.init))
    Reversed(poly, init)
end

immutable Padded{A<:U}<:Algorithm{A}
    pad_p::Int
    poly::A
    carry::A
    pad_8::Int
    init::A
end

function Padded{P<:U}(spec::Spec{P})
    A = fastest(P, Uint8)
    pad_p = pad(A, spec.width)
    poly = convert(A, spec.poly) << pad_p
    carry = one(A) << pad(A, 1)
    pad_8 = pad(A, 8)
    init = convert(A, spec.init) << pad_p
    Padded(pad_p, poly, carry, pad_8, init)
end

function crc_new{P<:U}(spec::Spec{P}; lookup=true)
    algo = spec.refin ? Reversed(spec) : Padded(spec)
    crc_new(spec, algo; lookup=lookup)
end

function crc_new{P<:U, A<:U}(spec::Spec{P}, algo::Algorithm{A}; lookup=true)
    if lookup
        tables = spec.refin ? Multiple{A}() : Single{A}()
        tables = make_tables(spec, algo, tables)
    else
        tables = NoTable()
    end
    return data -> finalize(spec, algo, 
                            extend(spec, algo, tables, data, algo.init))
end

function make_tables{A<:U}(spec, algo::Padded{A}, tables::Single{A})
    tables.table = Array(A, 256)
    for index in zero(Uint8):convert(Uint8, 255)
        remainder::A = convert(A, index) << algo.pad_8
        for _ in 1:8
            if remainder & algo.carry == algo.carry
                remainder = (remainder << 1) $ algo.poly
            else
                remainder <<= 1
            end
        end
        tables.table[index + 1] = remainder
    end
    tables
end

function make_tables{A<:U}(spec, algo::Reversed{A}, tables::Multiple{A})
    n_tables = sizeof(A)
    tables.tables = Vector{A}[Array(A, 256) for _ in 1:n_tables]
    for index in zero(Uint8):convert(Uint8, 255)
        remainder::A = convert(A, index)
        for _ in 1:8
            if remainder & one(A) == one(A)
                remainder = (remainder >>> 1) $ algo.poly
            else
               remainder >>>= 1
            end
        end
        tables.tables[1][index + 1] = remainder
    end
    for index in zero(Uint8):convert(Uint8, 255)
        remainder = tables.tables[1][index + 1]
        for t in 2:n_tables
            remainder = (remainder >>> 8) $ tables.tables[1][1 + (remainder & 0xff)]
            tables.tables[t][index + 1] = remainder
        end
    end
    tables
end

function finalize{P<:U, A<:U}(spec::Spec{P}, algo::Padded{A}, remainder::A)
    remainder = convert(P, remainder >> algo.pad_p)
    remainder = spec.refout ? reflect(spec.width, remainder) : remainder
    remainder $ spec.xorout
end

function finalize{P<:U, A<:U}(spec::Spec{P}, algo::Reversed{A}, remainder::A)
    remainder = spec.refout ? remainder : reflect(spec.width, remainder)
    convert(P, remainder) $ spec.xorout
end

function extend{P<:U, A<:U}(spec::Spec{P}, algo::Padded{A}, tables::NoTable, data::Vector{Uint8}, remainder::A)
    for word::Uint8 in data
        remainder::A = remainder $ (convert(A, word) << algo.pad_8)
        for _ in 1:8
            if remainder & algo.carry == algo.carry
                remainder = (remainder << 1) $ algo.poly
            else
                remainder <<= 1
            end
        end
    end
    remainder
end

function extend{P<:U, A<:U}(spec::Spec{P}, algo::Padded{A}, tables::Single{A}, data::Vector{Uint8}, remainder::A)
    for word::Uint8 in data
        remainder::A = remainder $ (convert(A, word) << algo.pad_8)
        remainder = (remainder << 8) $ tables.table[1 + remainder >>> algo.pad_8]
    end
    remainder
end

function extend{P<:U, A<:U}(spec::Spec{P}, algo::Reversed{A}, tables::NoTable, data::Vector{Uint8}, remainder::A)
    for word::Uint8 in data
        remainder::A = remainder $ convert(A, word)
        for _ in 1:8
            if remainder & one(A) == one(A)
                remainder = (remainder >>> 1) $ algo.poly
            else
                remainder >>>= 1
            end
        end
    end
    remainder
end

function extend{P<:U, A<:U}(spec::Spec{P}, algo::Reversed{A}, tables::Single{A}, data::Vector{Uint8}, remainder::A)
    for word::Uint8 in data
        remainder::A = remainder $ (convert(A, word))
        remainder = (remainder >>> 8) $ tables.table[1 + remainder & 0xff]
    end
    remainder
end

# this special case short-circuits "all Uint8" avoiding a
# self-recusrive loop below
function extend{P<:U}(spec::Spec{P}, algo::Reversed{Uint8}, tables::Multiple{Uint8}, data::Vector{Uint8}, remainder::Uint8)
    extend(spec, algo, Single(tables), data, remainder)
end

function extend{P<:U, A<:U}(spec::Spec{P}, algo::Reversed{A}, tables::Multiple{A}, data::Vector{A}, remainder::A)
    n_tables = length(tables.tables)
    for word::A in data
        tmp::A = remainder $ convert(A, word)
        remainder = zero(A)
        # TODO - unroll statically
        for t in 1:n_tables
            remainder $= tables.tables[t][(tmp >>> (n_tables - t)*8) & 0xff + 1]
        end
    end
    remainder
end

function extend{P<:U, A<:U}(spec::Spec{P}, algo::Reversed{A}, tables::Multiple{A}, data::Vector{Uint8}, remainder::A)
    tail = length(data) % sizeof(A)
    remainder = extend(spec, algo, tables, reinterpret(A, data), remainder)
    extend(spec, algo, Single(tables), data[end-tail+1:end], remainder)
end

end
