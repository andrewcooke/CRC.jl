
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

export crc, make_tables, TEST, CRC_3_ROHC, CRC_4_ITU, CRC_5_EPC,
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

#export to_uint, crc_no_table, make_table, crc_word_table,
#       crc_small_table, crc_large_table, Spec, crc, ReflectWords,
#       reflect, TEST, CRC_3_ROHC, CRC_4_ITU, CRC_5_EPC, CRC_5_ITU,
#       CRC_5_USB, CRC_6_CDMA2000_A, CRC_6_CDMA2000_B, CRC_6_DARC,
#       CRC_6_ITU, CRC_7, CRC_7_ROHC, CRC_8, CRC_8_CDMA2000,
#       CRC_8_DARC, CRC_8_DVB_S2, CRC_8_EBU, CRC_8_I_CODE, CRC_8_ITU,
#       CRC_8_MAXIM, CRC_8_ROHC, CRC_8_WCDMA, CRC_10, CRC_10_CDMA2000,
#       CRC_11, CRC_12_3GPP, CRC_12_CDMA2000, CRC_12_DECT, CRC_13_BBC,
#       CRC_14_DARC, CRC_15, CRC_15_MPT1327, CRC_16_ARC,
#       CRC_16_AUG_CCITT, CRC_16_BUYPASS, CRC_16_CCITT_FALSE,
#       CRC_16_CDMA2000, CRC_16_DDS_110, CRC_16_DECT_R, CRC_16_DECT_X,
#       CRC_16_DNP, CRC_16_EN_13757, CRC_16_GENIBUS, CRC_16_MAXIM,
#       CRC_16_RIELLO, CRC_16_TELEDISK, CRC_16_USB, CRC_16_CRC_A,
#       CRC_16_KERMIT, CRC_16_MODBUS, CRC_16_X_25, CRC_16_XMODEM,
#       CRC_24, CRC_24_FLEXRAY_A, CRC_24_FLEXRAY_B, CRC_31_PHILIPS,
#       CRC_32, CRC_32_BZIP2, CRC_32_C, CRC_32_D, CRC_32_MPEG_2,
#       CRC_32_POSIX, CRC_32_Q, CRC_32_JAMCRC, CRC_32_XFER, CRC_40_GSM,
#       CRC_64, CRC_64_WE, CRC_64_XZ, CRC_82_DARC

#import Base: start, done, next


# types and bit sizes
#   D/word_size - the input data words (nearly always Uint8)
#   P - used to store the polynomial (msb optional) and check, also used as
#       the type of the result
#   A/width - accumulator / remainder and table value (all vars in inner loop)
#             (for table-driven routines this is fixed by the table type)
#   degree - number of (least significant) bits in P used to specific poly
#            (plus an implicit msb)
#   all lookup tables have an 8-bit (byte) index.

typealias U Unsigned






TEST = b"123456789"

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

INDEX_SIZE = 8
TABLE_SIZE = 256

function make_tables{D<:U, A<:U, P<:U
                     }(::Type{D}, ::Type{A}, degree, poly::P, refin)

    word_size = 8 * sizeof(D)
    n_tables = word_size / INDEX_SIZE
    tables = Vector{A}[Array(A, TABLE_SIZE) for _ in 1:n_tables]
    poly = reflect(degree, poly)

    for index in zero(Uint8):convert(Uint8, TABLE_SIZE-1)
        remainder::A = convert(A, index)
        for _ in 1:INDEX_SIZE
            if remainder & one(A) == one(A)
                remainder = (remainder >>> 1) $ poly
            else
               remainder >>>= 1
            end
        end
        tables[1][(refin ? index : reflect(index)) + 1] = remainder
    end
    for index in zero(Uint8):convert(Uint8, TABLE_SIZE-1)
        remainder = tables[1][(refin ? index : reflect(index)) + 1]
        for t in 2:n_tables
            remainder = (remainder >>> INDEX_SIZE) $ tables[1][1 + (remainder & 0xff)]
            tables[t][(refin ? index : reflect(index)) + 1] = remainder
        end
    end
    tables
end

function extend{P<:U, A<:U}(poly::P, degree, refin, remainder::P, data, tables::Vector{Vector{A}})

    D = itype(data, Uint8)
    @assert largest(D, P, A) == A

    pad = refin ? 0 : 8 * sizeof(A) - degree
    poly = refin ? reflect(degree, poly) : poly
    poly = convert(A, poly) << pad
    remainder = refin ? reflect(degree, remainder) : remainder
    remainder = convert(A, remainder) << pad

    if length(tables) == 0
        if refin
            remainder = loop_no_tables_refin(D, poly, remainder, data)
        else
            remainder = loop_no_tables(D, poly, remainder, data)
        end
    else
        error("tables")
    end
    
    remainder >>>= pad
    remainder = refin ? reflect(degree, remainder) : remainder
    # TODO - is this mask ever needed?
#    mask::A = convert(A, (one(Uint128) << degree) - 1)
#    convert(P, remainder & mask)
    convert(P, remainder)
end

function loop_no_tables_refin{D<:U, A<:U
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

function loop_no_tables{D<:U, A<:U
                        }(::Type{D}, poly::A, remainder::A, data)
    shift = 8*sizeof(A) - 8*sizeof(D)
    carry = one(A) << (8*sizeof(A) - 1)
    for word::D in data
        remainder::A = remainder $ (convert(A, word) << shift)
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

function finalize_remainder(remainder, degree, refout, xorout)
    remainder = refout ? reflect(degree, remainder) : remainder
    remainder $ xorout
end


function crc{P<:U}(spec::Spec{P}, data)
    A = fastest(P, itype(data, Uint8))
    finalize_remainder(extend(spec.poly, spec.width, spec.refin, spec.init, data, Vector{A}[]), spec.width, spec.refout, spec.xorout)
end


































#function to_uint(size_or_type)
#    if isa(size_or_type, Type) && issubtype(size_or_type, U) && isleaftype(siz#e_or_type)
#        return size_or_type
#    elseif isa(size_or_type, Integer) && size_or_type > 0
#        if size_or_type <= 8
#            return Uint8
#        elseif size_or_type <= 16
#            return Uint16
#        elseif size_or_type <= 32
#            return Uint32
#        elseif size_or_type <= 64
#            return Uint64
#        elseif size_or_type <= 128
#            return Uint128
#        end
#    end
#    error("unexpected type / size: $size_or_type ($(typeof(size_or_type)))")
#end
#
#function largest(T, TS...)
#    big = to_uint(T)
#    for t in map(to_uint, TS)
#        if sizeof(t) > sizeof(big)
#            big = t
#        end
#    end
#    big
#end
#
#function fastest(T, TS...)
#    l = largest(T, TS...)
#    if l == Uint32 && Uint32 != Uint
#        Uint64  # round up for speed
#    else
#        l
#    end
#end
#
#function itype(iterable, default)
#    for i in iterable
#        return typeof(i)
#    end
#    return default
#end
#
#
## common support for calculations
#
#function check_poly{A<:U, P<:U
#                    }(::Type{A}, degree, poly::P, init, refin, xorout)
#
#    @assert largest(A, degree) == A "accumulator $A too narrow for degree $deg#ree"
#    @assert largest(P, degree) == P "polynomial $P too narrow for degree $degr#ee"
#
#    # how many spaces to right of polynomial in accumulator while
#    # doing the division
#    width = 8 * sizeof(A)
#    pad = refin ? 0 : width - degree
#
#    # carry before shift on padded data
#    carry::A = refin ? 1 : one(A) << (width - 1)
#
#    # isolate the polynomial (except implicit msb) after padding removed
#    rem_mask::A = convert(A, (one(Uint128) << degree) - 1)
#
#    # init and poly both need converting and then padding or reflecting
#    init::A = convert(A, init & rem_mask) << pad
#    init = refin ? reflect(degree, init) : init
#    poly::A = convert(A, poly & rem_mask) << pad
#    poly = refin ? reflect(degree, poly) : poly
#
#    xorout = convert(P, xorout)
#    (poly, init, width, pad, carry, rem_mask, xorout)
#end
#
#function check_poly{D<:U, A<:U, P<:U
#                    }(::Type{D}, ::Type{A}, degree, poly::P, init, refin, xoro#ut)
#
#    @assert largest(D, A) == A "accumulator too narrow for data"
#    poly, init, width, pad, carry, rem_mask, xorout = 
#        check_poly(A, degree, poly, init, refin, xorout)
#
#    # the shift when we load a data word into the remainder / accumulator
#    word_size = 8 * sizeof(D)
#    load = refin ? 0 : width - word_size
#
#    (poly, init, width, pad, carry, rem_mask, load, word_size, xorout)
#end
#
#function fix_remainder{P<:U
#                       }(::Type{P}, degree, remainder, rem_mask, pad, refin, r#efout, xorout::P)
#    remainder = convert(P, (remainder >>> pad) & rem_mask)
#    remainder = (refin $ refout) ? reflect(degree, remainder) : remainder
#    remainder $ xorout
#end
#
#function check_data{D<:U}(::Type{D}, data)
#    @assert itype(data, D) == D "data not of correct size ($D / $(typeof(d))))#"
#end
#
#
## basic calculation without a table
#
#function crc_no_table{D<:U, A<:U, P<:U
#                      }(::Type{D}, ::Type{A}, degree, poly::P, data; 
#                        init=0, refin=false, refout=false, xorout=0)
#    check_data(D, data)
#    poly::A, init::A, width, pad, carry::A, rem_mask::A, load, word_size, xoro#ut::P = 
#        check_poly(D, A, degree, poly, init, refin, xorout)
#    if refin
#        remainder = loop_no_table_ref(D, poly, init, data, word_size)
#    else
#        remainder = loop_no_table(D, poly, init, data, carry, word_size, load)
#    end
#    fix_remainder(P, degree, remainder, rem_mask, pad, refin, refout, xorout)
#end
#
#crc_no_table{D<:U, P<:U
#             }(degree, poly::P, data::Vector{D}; 
#               init=0, refin=false, refout=false, xorout=0) = 
#               crc_no_table(D, fastest(D, degree), degree, poly, data, 
#                            init=init, refin=refin, refout=refout, xorout=xoro#ut)
#
#function loop_no_table_ref{D<:U, A<:U
#                           }(::Type{D}, poly::A, remainder::A, data, word_size#)
#    for word::D in data
#        remainder::A = remainder $ convert(A, word)
#        for _ in 1:word_size
#            if remainder & one(A) == one(A)
#                remainder = (remainder >>> 1) $ poly
#            else
#                remainder >>>= 1
#            end
#        end
#    end
#    remainder
#end
#
#function loop_no_table{D<:U, A<:U
#                       }(::Type{D}, poly::A, remainder::A, data, carry::A, wor#d_size, load)
#    for word::D in data
#        remainder::A = remainder $ (convert(A, word) << load)
#        for _ in 1:word_size
#            if remainder & carry == carry
#                remainder = (remainder << 1) $ poly
#            else
#                remainder <<= 1
#            end
#        end
#    end
#    remainder
#end
#
#
## generate lookup tables.  first is the "real" table; subsequent are 
## repeated evaluation against zero data to shift by a byte (allowing 
## multiple bytes to be merged for large words)
#
#INDEX_SIZE = 8
#TABLE_SIZE = 256
#
#function make_tables{D<:U, A<:U, P<:U
#                     }(::Type{D}, ::Type{A}, degree, poly::P;
#                      refin=false)
#
#    @assert largest(A, Uint8) == A "accumulator too narrow for index"
#
#    word_size = 8 * sizeof(D)
#    n_tables = word_size / INDEX_SIZE
#    table = Vector{A}[Array(A, TABLE_SIZE) for _ in 1:n_tables]
#    poly = reflect(degree, poly)
#
#    for index in zero(Uint8):convert(Uint8, TABLE_SIZE-1)
#        remainder::A = convert(A, index)
#        for _ in 1:INDEX_SIZE
#            if remainder & one(A) == one(A)
#                remainder = (remainder >>> 1) $ poly
#            else
#               remainder >>>= 1
#            end
#        end
#        if refin
#            table[1][index+1] = remainder
#        else
#            table[1][reflect(index)+1] = reflect(remainder)
#        end
#        for t in 2:n_tables
#            remainder = (remainder >>> INDEX_SIZE) $ table[1][1 + (remainder &# 0xff)])
#            if refin
#                table[t][index+1] = remainder
#            else
#                table[t][reflect(index)+1] = reflect(remainder)
#            end
#        end
#    end
#    tables
#end
#
#
## use a table whose index matches the size of the input data words.
#
#function crc_word_table{D<:U, A<:U, P<:U
#                        }(::Type{D}, degree, poly::P, data, table::Vector{A}; 
#                          init=0, refin=false, refout=false, xorout=0)
#    check_data(D, data)
#    poly::A, init::A, width, pad, carry::A, rem_mask::A, load, word_size, xoro#ut::P = 
#        check_poly(D, A, degree, poly, init, refin, xorout)
#    index_size = measure_table(table)
#    @assert word_size == index_size "incorrect index size (not word)"
#    if refin
#        remainder = loop_word_ref(D, init, data, table, word_size)
#    else
#        remainder = loop_word(D, init, data, table, load, word_size)
#    end
#    fix_remainder(P, degree, remainder, rem_mask, pad, refin, refout, xorout)
#end
#
#crc_word_table{D<:U, A<:U, P<:U
#               }(degree, poly::P, data::Vector{D}, table::Vector{A};
#                 init=0, refin=false, refout=false, xorout=0) = 
#                 crc_word_table(D, degree, poly, data, table, 
#                                init=init, refin=refin, refout=refout, xorout=#xorout)
#
#function loop_word_ref{D<:U, A<:U
#                       }(::Type{D}, remainder::A, data, table::Vector{A}, word#_size)
#    for word::D in data
#        word $= convert(D, remainder)
#        remainder = (remainder >>> word_size) $ table[1 + word]
#    end
#    remainder
#end
#
#function loop_word{D<:U, A<:U
#                   }(::Type{D}, remainder::A, data, table::Vector{A}, load, wo#rd_size)
#    for word::D in data
#        word $= convert(D, remainder >>> load)
#        remainder = (remainder << word_size) $ table[1 + word]
#    end
#    remainder
#end
#
#
#
#
## http://stackoverflow.com/questions/2602823/in-c-c-whats-the-simplest-way-to-#reverse-the-order-of-bits-in-a-byte
#function reflect_bits(n::Uint8)
#    n = (n & 0xf0) >>> 4 | (n & 0x0f) << 4;
#    n = (n & 0xcc) >>> 2 | (n & 0x33) << 2;
#    (n & 0xaa) >>> 1 | (n & 0x55) << 1;
#end
#
#REFLECT_8 = Uint8[reflect_bits(i) for i in 0x00:0xff]
#
#reflect(u::Uint8) = REFLECT_8[u+1]
#
#for (T,S) in ((Uint16, Uint8), (Uint32, Uint16), (Uint64, Uint32), (Uint128, U#int64))
#    n = 8 * sizeof(S)
#    mask::S = -one(S)
#    @eval reflect(u::$T) = (convert($T, reflect(convert($S, u & $mask))) << $n#) | reflect(convert($S, (u >>> $n) & $mask))
#end
#
#function reflect{T<:U}(size, u::T)
#    width = 8 * sizeof(T)
#    @assert size <= width "cannot reflect a value larger than the representati#on"
#    u = reflect(u) >>> (width - size)
#end
#
## automatically reflect individual words on iterations
#
#immutable ReflectWords{T}
#    inner::T
#end
#
#start{T}(r::ReflectWords{T}) = start(r.inner)
#done{T}(r::ReflectWords{T}, state) = done(r.inner, state)
#
#function next{T}(r::ReflectWords{T}, state)
#    i, state = next(r.inner, state)
#    reflect(i), state
#end
#
#
#
#
#
#
## table inside closure - allows for re-use of table
#
#function crc{D<:U, A<:U, P<:U}(spec::Spec{P}, ::Type{D}, ::Type{A}; 
#                               table=true, index_size=8)
#    if table
#        t = make_table(A, spec.awidth, spec.poly, index_size, refin=spec.refin)
#        word_size = 8 * sizeof(D)
#        if word_size == index_size
#            data -> crc_word_table(D, spec.width, spec.poly, data, t,
#                                   init=spec.init, refin=spec.refin, refout=sp#ec.refout, xorout=spec.xorout)
#        elseif word_size > index_size
#            data -> crc_small_table(D, spec.width, spec.poly, data, t, 
#                                    init=spec.init, refin=spec.refin, refout=s#pec.refout, xorout=spec.xorout)
#        else
#            data -> crc_large_table(D, spec.width, spec.poly, data, t,
#                                    init=spec.init, refin=spec.refin, refout=s#pec.refout, xorout=spec.xorout)
#        end
#    else
#        data -> crc_no_table(D, A, spec.width, spec.poly, data,
#                             init=spec.init, refin=spec.refin, refout=spec.ref#out, xorout=spec.xorout)
#    end
#end
#
#crc{D<:U, P<:U
#    }(spec::Spec{P}, ::Type{D}; table=true, index_size=8) =
#        crc(spec, D, fastest(D, P, index_size))
#
#
## single shot (table not cached)
#
#crc{D<:U, A<:U, P<:U}(spec::Spec{P}, data::Vector{D}, ::Type{A};
#                      table=true, index_size=8) =
#    crc(spec, D, A, table=table, index_size=index_size)(data)
#
#crc{D<:U, P<:U}(spec::Spec{P}, data::Vector{D};
#                table=true, index_size=8) =
#                    crc(spec, data, fastest(D, P, index_size), 
#                        table=table, index_size=index_size)


end
