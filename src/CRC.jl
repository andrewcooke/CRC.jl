
# calculate crc checksums for data.  this process is closely related
# to the routines in IntModN (particularly GF2Poly), but the
# traditional implementations are optimized to the point where
# implementing them from "first principles" makes little sense
# (although we can use IntModN to check the results here).

# the basic idea is to do a polynomial division along the byte stream.
# it's all explained very nicely at
# http://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
# and http://www.zlib.net/crc_v3.txt

# TODO - would @inbounds help in the loops?


module CRC

export to_uint, rem_no_table, make_table, rem_word_table,
       rem_small_table, rem_large_table, Std, crc, ReflectWords,
       reflect, TEST, CRC_3_ROHC, CRC_4_ITU, CRC_5_EPC, CRC_5_ITU,
       CRC_5_USB, CRC_6_CDMA2000_A, CRC_6_CDMA2000_B, CRC_6_DARC,
       CRC_6_ITU, CRC_7, CRC_7_ROHC, CRC_8, CRC_8_CDMA2000,
       CRC_8_DARC, CRC_8_DVB_S2, CRC_8_EBU, CRC_8_I_CODE, CRC_8_ITU,
       CRC_8_MAXIM, CRC_8_ROHC, CRC_8_WCDMA, CRC_10, CRC_10_CDMA2000,
       CRC_11, CRC_12_3GPP, CRC_12_CDMA2000, CRC_12_DECT, CRC_13_BBC,
       CRC_14_DARC, CRC_15, CRC_15_MPT1327, CRC_16_ARC,
       CRC_16_AUG_CCITT, CRC_16_BUYPASS, CRC_16_CCITT_FALSE,
       CRC_16_CDMA2000, CRC_16_DDS_110, CRC_16_DECT_R,
       CRC_16_EN_13757, CRC_16_GENIBUS, CRC_16_MAXIM, CRC_16_RIELLO,
       CRC_16_TELEDISK, CRC_16_USB, CRC_16_CRC_A, CRC_16_KERMIT,
       CRC_16_MODBUS, CRC_16_X_25, CRC_16_XMODEM, CRC_24,
       CRC_24_FLEXRAY_A, CRC_24_FLEXRAY_B, CRC_31_PHILIPS, CRC_32,
       CRC_32_BZIP2, CRC_32_C, CRC_32_D, CRC_32_MPEG_2, CRC_32_POSIX,
       CRC_32_Q, CRC_32_JAMCRC, CRC_32_XFER, CRC_40_GSM, CRC_64,
       CRC_64_WE, CRC_64_XZ, CRC_82_DARC

import Base: start, done, next


# types and bit sizes
#   D/word_size - the input data words (nearly always Uint8)
#   P - used to store the polynomial (msb optional) and check, also used as
#       the type of the result
#   A/width - accumulator / remainder and table value (all vars in inner loop)
#             (for table-driven routines this is fixed by the table type)
#   index_size - number of (most significant) bits from A used to index table
#                (this must be an exact multiple / divisor of word_size)
#   degree - number of (least significant) bits in P used to specific poly
#            (plus an implicit msb)

typealias U Unsigned

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


# common support for calculations

function check_poly{A<:U, P<:U
                    }(::Type{A}, degree, poly::P, init, refin)

    @assert largest(A, degree) == A "accumulator too narrow for degree"
    @assert largest(P, degree) == P "polynomial too narrow for degree"

    # how many spaces to right of polynomial in accumulator while
    # doing the division
    width = 8 * sizeof(A)
    pad = refin ? 0 : width - degree

    # carry before shift on padded data
    carry::A = refin ? 1 : one(A) << (width - 1)

    # isolate the polynomial (except implicit msb) after padding removed
    rem_mask::A = convert(A, (one(Uint128) << degree) - 1)

    # init and poly both need converting and then padding or reflecting
    init::A = convert(A, init & rem_mask) << pad
    init = refin ? reflect(degree, init) : init
    poly::A = convert(A, poly & rem_mask) << pad
    poly = refin ? reflect(degree, poly) : poly

    (poly, init, width, pad, carry, rem_mask)
end

function check_poly{D<:U, A<:U, P<:U
                    }(::Type{D}, ::Type{A}, degree, poly::P, init, refin)

    @assert largest(D, A) == A "accumulator too narrow for data"
    poly, init, width, pad, carry, rem_mask = check_poly(A, degree, poly, init, refin)

    # the shift when we load a data word into the remainder / accumulator
    word_size = 8 * sizeof(D)
    load = refin ? 0 : width - word_size

    (poly, init, width, pad, carry, rem_mask, load, word_size)
end

function fix_remainder{P<:U
                       }(::Type{P}, degree, remainder, rem_mask, pad, refin, refout)
    remainder = convert(P, (remainder >>> pad) & rem_mask)
    (refin $ refout) ? reflect(degree, remainder) : remainder
end

function measure_table{A<:U}(table::Vector{A})
    index_size = iround(log2(length(table)))
    @assert 2 ^ index_size == length(table) "incorrect table size"
    @assert index_size <= 8 * sizeof(A) "index wider than accumulator"
    index_size
end


# basic calculation without a table

function rem_no_table{D<:U, A<:U, P<:U
                      }(::Type{D}, ::Type{A}, degree, poly::P, data; 
                        init=0, refin=false, refout=false)
    poly::A, init::A, width, pad, carry::A, rem_mask::A, load, word_size = 
        check_poly(D, A, degree, poly, init, refin)
    if refin
        remainder = loop_no_table_ref(D, poly, init, data, word_size)
    else
        remainder = loop_no_table(D, poly, init, data, carry, word_size, load)
    end
    fix_remainder(P, degree, remainder, rem_mask, pad, refin, refout)
end

rem_no_table{D<:U, P<:U
             }(degree, poly::P, data::Vector{D}; 
               init=0, refin=false, refout=false) = 
               rem_no_table(D, fastest(D, degree), degree, poly, data, 
                            init=init, refin=refin, refout=refout)

function loop_no_table_ref{D<:U, A<:U
                           }(::Type{D}, poly::A, remainder::A, data, word_size)
    for word::D in data
        remainder::A = remainder $ convert(A, word)
        for _ in 1:word_size
            if remainder & one(A) == one(A)
                remainder = (remainder >>>= 1) $ poly
            else
                remainder >>>= 1
            end
        end
    end
    remainder
end

function loop_no_table{D<:U, A<:U
                       }(::Type{D}, poly::A, remainder::A, data, carry::A, word_size, load)
    for word::D in data
        remainder::A = remainder $ (convert(A, word) << load)
        for _ in 1:word_size
            if remainder & carry == carry
                remainder = (remainder << 1) $ poly
            else
                remainder <<= 1
            end
        end
    end
    remainder
end


# generate a lookup table of the requested size

function make_table{A<:U, P<:U
                    }(::Type{A}, degree, poly::P, index_size;
                      refin=false)

    @assert index_size < 33 "table too large"  # even this is huge
    @assert largest(A, index_size) == A "accumulator too narrow for index"

    poly::A, init::A, width, pad, carry::A, rem_mask::A = 
        check_poly(A, degree, poly, 0, false)  # reflect below
    index_shift = width - index_size
    table_size = 2 ^ index_size
    table = Array(A, table_size)

    # TODO - reverse logic as refin case simpler and faster
    for index in zero(Uint64):convert(Uint64, table_size-1)
        remainder::A = convert(A, index) << index_shift
        for _ in 1:index_size
            if remainder & carry == carry
                remainder = (remainder << 1) $ poly
            else
                remainder <<= 1
            end
        end
        if refin
            table[reflect(index_size, index)+1] = reflect(remainder)
        else
            table[index+1] = remainder
        end
    end
    table
end


# use a table whose index matches the size of the input data words.

function rem_word_table{D<:U, A<:U, P<:U
                        }(::Type{D}, degree, poly::P, data, table::Vector{A}; 
                          init=0, refin=false, refout=false)
    poly::A, init::A, width, pad, carry::A, rem_mask::A, load, word_size = 
        check_poly(D, A, degree, poly, init, refin)
    index_size = measure_table(table)
    @assert word_size == index_size "incorrect index size (not word)"
    if refin
        remainder = loop_word_ref(D, init, data, table, word_size)
    else
        remainder = loop_word(D, init, data, table, load, word_size)
    end
    fix_remainder(P, degree, remainder, rem_mask, pad, refin, refout)
end

rem_word_table{D<:U, A<:U, P<:U
               }(degree, poly::P, data::Vector{D}, table::Vector{A};
                 init=0, refin=false, refout=false) = 
                 rem_word_table(D, degree, poly, data, table, 
                                init=init, refin=refin, refout=refout)

function loop_word_ref{D<:U, A<:U
                       }(::Type{D}, remainder::A, data, table::Vector{A}, word_size)
    for word::D in data
        word $= convert(D, remainder)
        remainder = (remainder >>> word_size) $ table[1 + word]
    end
    remainder
end

function loop_word{D<:U, A<:U
                   }(::Type{D}, remainder::A, data, table::Vector{A}, load, word_size)
    for word::D in data
        word $= convert(D, remainder >>> load)
        remainder = (remainder << word_size) $ table[1 + word]
    end
    remainder
end


# use a table whose index is smaller than the size of the input data words
# (for efficiency it must be an exact divisor).

function rem_small_table{D<:U, A<:U, P<:U
                         }(::Type{D}, degree, poly::P, data, table::Vector{A};
                           init=0, refin=false, refout=false)

    poly::A, init::A, width, pad, carry::A, rem_mask::A, load, word_size = 
        check_poly(D, A, degree, poly, init, refin)
    index_size = measure_table(table)
    @assert word_size >= index_size "incorrect index size (not small)"
    @assert word_size % index_size == 0 "incorrect index size (not divisor of word size)"
    index_shift = refin ? 0 : width - index_size
    index_mask::A = convert(A, (one(Uint128) << index_size) - 1) << index_shift
    n_shifts = div(word_size, index_size)

    if refin
        remainder = loop_small_table_ref(D, init, data, table, n_shifts,
                                         index_mask, index_size)
    else
        remainder = loop_small_table(D, init, data, table, load, n_shifts,
                                     index_mask, index_size, index_shift)
    end
    fix_remainder(P, degree, remainder, rem_mask, pad, refin, refout)
end

rem_small_table{D<:U, A<:U, P<:U
                }(degree, poly::P, data::Vector{D}, table::Vector{A}; 
                  init=0, refin=false, refout=false) = 
                  rem_small_table(D, degree, poly, data, table, 
                                  init=init, refin=refin, refout=refout)

function loop_small_table_ref{D<:U, A<:U
                              }(::Type{D}, remainder::A, data, table::Vector{A},
                                n_shifts, index_mask, index_size)
    for word::D in data
        # TODO - can this be xored just once at the start?
        tmp::A = convert(A, word)
        for _ in 1:n_shifts
            remainder::A = remainder $ (tmp & index_mask)
            remainder = (remainder >>> index_size) $ table[1 + (remainder & index_mask)]
            tmp >>>= index_size
        end
    end
    remainder
end

function loop_small_table{D<:U, A<:U
                          }(::Type{D}, remainder::A, data, table::Vector{A}, 
                            load, n_shifts, index_mask, index_size, index_shift)
    for word::D in data
        # TODO - can this be xored just once at the start?
        tmp::A = convert(A, word) << load
        for _ in 1:n_shifts
            remainder::A = remainder $ (tmp & index_mask)
            remainder = (remainder << index_size) $ table[1 + (remainder >>> index_shift)]
            tmp <<= index_size
        end
    end
    remainder
end


# use a table whose index is larger than the size of the input data
# words (for efficiency it must be an exact multiple).

function rem_large_table{D<:U, A<:U, P<:U
                         }(::Type{D}, degree, poly::P, data, table::Vector{A};
                           init=0, refin=false, refout=false)

    poly::A, init::A, width, pad, carry::A, rem_mask::A, load, word_size = 
        check_poly(D, A, degree, poly, init, refin)
    index_size = measure_table(table)
    @assert word_size <= index_size "incorrect index size (not large)"
    @assert index_size % word_size == 0 "incorrect index size (not multiple of word size)"
    index_shift = refin ? 0 : width - index_size
    index_mask::A = convert(A, (one(Uint128) << index_size) - 1) << index_shift
    n_shifts = div(index_size, word_size)

    if refin
        remainder = loop_large_table_ref(D, init, data, table, word_size,
                                         n_shifts, index_size, index_mask)
    else
        remainder = loop_large_table(D, init, data, table, load, word_size,
                                     n_shifts, index_size, index_shift)
    end
    fix_remainder(P, degree, remainder, rem_mask, pad, refin, refout)
end

rem_large_table{D<:U, A<:U, P<:U
                }(degree, poly::P, data::Vector{D}, table::Vector{A}; 
                  init=0, refin=false, refout=false) = 
                  rem_large_table(D, degree, poly, data, table, 
                                  init=init, refin=refin, refout=refout)

function loop_large_table_ref{D<:U, A<:U
                              }(::Type{D}, remainder::A, data, table::Vector{A},
                                word_size, n_shifts, index_size, index_mask)
    iter = start(data)
    while !done(data, iter)
        for i in 1:n_shifts
            if !done(data, iter)
                word::D, iter = next(data, iter)
                shift = (i-1) * word_size
                remainder = remainder $ (convert(A, word) << shift)
            end
        end
        remainder = (remainder >>> index_size) $ table[1 + (remainder & index_mask)]
    end
    remainder
end

function loop_large_table{D<:U, A<:U
                          }(::Type{D}, remainder::A, data, table::Vector{A},
                            load, word_size, n_shifts, index_size, index_shift)
    iter = start(data)
    left_shift, right_shift = index_size, index_shift
    while !done(data, iter)
        for i in 1:n_shifts
            if !done(data, iter)
                word::D, iter = next(data, iter)
                shift = load - (i-1) * word_size
                remainder = remainder $ (convert(A, word) << shift)
            else
                left_shift -= word_size
                right_shift += word_size
            end
        end
        remainder = (remainder << left_shift) $ table[1 + (remainder >>> right_shift)]
    end
    remainder
end


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
    u = reflect(u) >>> (width - size)
end

# automatically reflect individual words on iterations

immutable ReflectWords{T}
    inner::T
end

start{T}(r::ReflectWords{T}) = start(r.inner)
done{T}(r::ReflectWords{T}, state) = done(r.inner, state)

function next{T}(r::ReflectWords{T}, state)
    i, state = next(r.inner, state)
    reflect(i), state
end


TEST = b"123456789"
MAX_INDEX_SIZE = 16
DEFAULT_A = Uint


type Std{A<:U, P<:U}

    # http://www.zlib.net/crc_v3.txt
    width::Int    # polynomial degree
    poly::P       # generating poly with msb missing
    init::P       # initial remainder
    refin::Bool   # reflect input
    refout::Bool  # reflect output
    xorout::P     # xored with final remainder
    test::P       # checksum for TEST

    index_size::Int  # number of bits used to index the lookup table
    table::Vector{A}

    # this leaves the table undefined, to be created on first use
    Std(width::Int, poly::P, init::P, refin::Bool, refout::Bool, xorout::P, test::P, index_size::Int) = new(width, poly, init, refin, refout, xorout, test, index_size)
    
end


function defaults(width)
    index_size = min(MAX_INDEX_SIZE, 8 * sizeof(to_uint(width)))
#    index_size = 8
    A = to_uint(width)
    A, index_size
end

function Std{P<:U}(width::Int, poly::P, init::P, refin::Bool, refout::Bool, xorout::P, test::P)
    A, index_size = defaults(width)
    Std{A, P}(width, poly, init, refin, refout, xorout, test, index_size)
end

Std{P<:U}(poly::P, init::P, refin::Bool, refout::Bool, xorout::P, test::P) = Std(8*sizeof(P), poly, init, refin, refout, xorout, test)


# http://reveng.sourceforge.net/crc-catalogue/1-15.htm
CRC_3_ROHC =         Std(3, 0x03, 0x07, true,  true,  0x00, 0x06)
CRC_4_ITU =          Std(4, 0x03, 0x00, true,  true,  0x00, 0x07)
CRC_5_EPC =          Std(5, 0x09, 0x09, false, false, 0x00, 0x00)
CRC_5_ITU =          Std(5, 0x15, 0x00, true,  true,  0x00, 0x07)
CRC_5_USB =          Std(5, 0x05, 0x1f, true,  true,  0x1f, 0x19)
CRC_6_CDMA2000_A =   Std(6, 0x27, 0x3f, false, false, 0x00, 0x0d)
CRC_6_CDMA2000_B =   Std(6, 0x07, 0x3f, false, false, 0x00, 0x3b)
CRC_6_DARC =         Std(6, 0x19, 0x00, true,  true,  0x00, 0x26)
CRC_6_ITU =          Std(6, 0x03, 0x00, true,  true,  0x00, 0x06)
CRC_7 =              Std(7, 0x09, 0x00, false, false, 0x00, 0x75)
CRC_7_ROHC =         Std(7, 0x4f, 0x7f, true,  true,  0x00, 0x53)
CRC_8 =              Std(0x07, 0x00, false, false, 0x00, 0xf4)
CRC_8_CDMA2000 =     Std(0x9b, 0xff, false, false, 0x00, 0xda)
CRC_8_DARC =         Std(0x39, 0x00, true,  true,  0x00, 0x15)
CRC_8_DVB_S2 =       Std(0xd5, 0x00, false, false, 0x00, 0xbc)
CRC_8_EBU =          Std(0x1d, 0xff, true,  true,  0x00, 0x97)
CRC_8_I_CODE =       Std(0x1d, 0xfd, false, false, 0x00, 0x7e)
CRC_8_ITU =          Std(0x07, 0x00, false, false, 0x55, 0xa1)
CRC_8_MAXIM =        Std(0x31, 0x00, true,  true,  0x00, 0xa1)
CRC_8_ROHC =         Std(0x07, 0xff, true,  true,  0x00, 0xd0)
CRC_8_WCDMA =        Std(0x9b, 0x00, true,  true,  0x00, 0x25)
CRC_10 =             Std(10, 0x0233, 0x0000, false, false, 0x0000, 0x0199)
CRC_10_CDMA2000 =    Std(10, 0x03d9, 0x03ff, false, false, 0x0000, 0x0233)
CRC_11 =             Std(11, 0x0385, 0x001a, false, false, 0x0000, 0x05a3)
CRC_12_3GPP =        Std(12, 0x080f, 0x0000, false, true,  0x0000, 0x0daf)
CRC_12_CDMA2000 =    Std(12, 0x0f13, 0x0fff, false, false, 0x0000, 0x0d4d)
CRC_12_DECT =        Std(12, 0x080f, 0x0000, false, false, 0x0000, 0x0f5b)
CRC_13_BBC =         Std(13, 0x1cf5, 0x0000, false, false, 0x0000, 0x04fa)
CRC_14_DARC =        Std(14, 0x0805, 0x0000, true,  true,  0x0000, 0x082d)
CRC_15 =             Std(15, 0x4599, 0x0000, false, false, 0x0000, 0x059e)
CRC_15_MPT1327 =     Std(15, 0x6815, 0x0000, false, false, 0x0001, 0x2566)

# http://reveng.sourceforge.net/crc-catalogue/16.htm#crc.cat.crc-16-ccitt-false
CRC_16_ARC =         Std(0x8005, 0x0000, true,  true,  0x0000, 0xbb3d)
CRC_16_AUG_CCITT =   Std(0x1021, 0x1d0f, false, false, 0x0000, 0xe5cc)
CRC_16_BUYPASS   =   Std(0x8005, 0x0000, false, false, 0x0000, 0xfee8)
CRC_16_CCITT_FALSE = Std(0x1021, 0xffff, false, false, 0x0000, 0x29b1)
CRC_16_CDMA2000 =    Std(0xc867, 0xffff, false, false, 0x0000, 0x4c06)
CRC_16_DDS_110 =     Std(0x8005, 0x800d, false, false, 0x0000, 0x9ecf)
CRC_16_DECT_R =      Std(0x0589, 0x0000, false, false, 0x0001, 0x007e)
CRC_16_DECT_X =      Std(0x0589, 0x0000, false, false, 0x0000, 0x007f)
CRC_16_DNP =         Std(0x3d65, 0x0000, true,  true,  0xffff, 0xea82)
CRC_16_EN_13757 =    Std(0x3d65, 0x0000, false, false, 0xffff, 0xc2b7)
CRC_16_GENIBUS =     Std(0x1021, 0xffff, false, false, 0xffff, 0xd64e)
CRC_16_MAXIM =       Std(0x8005, 0x0000, true,  true,  0xffff, 0x44c2)
CRC_16_RIELLO =      Std(0x8bb7, 0x0000, false, false, 0x0000, 0xd0db)
CRC_16_TELEDISK =    Std(0x1021, 0x89ec, true,  true,  0x0000, 0x26b1)
CRC_16_USB =         Std(0x8005, 0xffff, true,  true,  0xffff, 0xb4c8)
CRC_16_CRC_A =       Std(0x1021, 0xc6c6, true,  true,  0x0000, 0xbf05)
CRC_16_KERMIT =      Std(0x1021, 0x0000, true,  true,  0x0000, 0x2189)
CRC_16_MODBUS =      Std(0x8005, 0xffff, true,  true,  0x0000, 0x4b37)
CRC_16_X_25 =        Std(0x1021, 0xffff, true,  true,  0xffff, 0x906e)
CRC_16_XMODEM =      Std(0x1021, 0x0000, false, false, 0x0000, 0x31c3)

# http://reveng.sourceforge.net/crc-catalogue/17plus.htm
CRC_24 =             Std(24, 0x00864cfb, 0x00b704ce, false, false, 0x00000000, 0x0021cf02)
CRC_24_FLEXRAY_A =   Std(24, 0x005d6dcb, 0x00fedcba, false, false, 0x00000000, 0x007979bd)
CRC_24_FLEXRAY_B =   Std(24, 0x005d6dcb, 0x00abcdef, false, false, 0x00000000, 0x001f23b8)
CRC_31_PHILIPS =     Std(31, 0x04c11db7, 0x7fffffff, false, false, 0x7fffffff, 0x0ce9e46c)
CRC_32 =             Std(0x04c11db7, 0xffffffff, true,  true,  0xffffffff, 0xcbf43926)
CRC_32_BZIP2 =       Std(0x04c11db7, 0xffffffff, false, false, 0xffffffff, 0xfc891918)
CRC_32_C =           Std(0x1edc6f41, 0xffffffff, true,  true,  0xffffffff, 0xe3069283)
CRC_32_D =           Std(0xa833982b, 0xffffffff, true,  true,  0xffffffff, 0x87315576)
CRC_32_MPEG_2 =      Std(0x04c11db7, 0xffffffff, false, false, 0x00000000, 0x0376e6e7)
CRC_32_POSIX =       Std(0x04c11db7, 0x00000000, false, false, 0xffffffff, 0x765e7680)
CRC_32_Q =           Std(0x814141ab, 0x00000000, false, false, 0x00000000, 0x3010bf7f)
CRC_32_JAMCRC =      Std(0x04c11db7, 0xffffffff, true,  true,  0x00000000, 0x340bc6d9)
CRC_32_XFER =        Std(0x000000af, 0x00000000, false, false, 0x00000000, 0xbd0be338)
CRC_40_GSM =         Std(40, 0x0000000004820009, 0x0000000000000000, false, false, 0x000000ffffffffff, 0x000000d4164fc646)
CRC_64 =             Std(0x42f0e1eba9ea3693, 0x0000000000000000, false, false, 0x0000000000000000, 0x6c40df5f0b497347)
CRC_64_WE =          Std(0x42f0e1eba9ea3693, 0xffffffffffffffff, false, false, 0xffffffffffffffff, 0x62ec59e3f1a4f00a)
CRC_64_XZ =          Std(0x42f0e1eba9ea3693, 0xffffffffffffffff, true,  true,  0xffffffffffffffff, 0x995dc9bbdf1939fa)
CRC_82_DARC =        Std(82, 0x0308c0111011401440411, 0x000000000000000000000, true,  true,   0x000000000000000000000, 0x09ea83f625023801fd612)


function crc{D<:U, A<:U, P<:U}(::Type{D}, std::Std{A, P}, data)
    if !isdefined(std, :table)
        std.table = make_table(A, std.width, std.poly, std.index_size)
    end
    if std.refin
        data = ReflectWords(data)
    end
    word_size = 8 * sizeof(D)
    if word_size == std.index_size
        remainder = rem_word_table(D, std.width, std.poly, data, std.table; init=std.init)
    elseif word_size > std.index_size
        remainder = rem_small_table(D, std.width, std.poly, data, std.table; init=std.init)
    else
        remainder = rem_large_table(D, std.width, std.poly, data, std.table; init=std.init)
    end
    if std.refout
        remainder = reflect(std.width, remainder)
    end
    remainder $ std.xorout
end

crc{D<:U, A<:U, P<:U}(std::Std{A, P}, data::Vector{D}) = crc(D, std, data)


end
