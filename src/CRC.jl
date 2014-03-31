
# calculate crc checksums for data.  this process is closely related
# to the routines in IntModN (particularly GF2Poly), but the
# traditional implementations are optimized to the point where
# implementing them from "first principles" makes little sense
# (although we can use IntModN to check the results here).

# the basic idea is to do a polynomial division along the byte stream.
# it's all explained very nicely at
# http://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks

module CRC

export rem_no_table, make_table, rem_word_table, rem_small_table,
       rem_big_table, Std, crc, ReflectWords, reflect, TEST,
       CRC_16_ARC, CRC_16_AUG_CCITT, CRC_16_BUYPASS,
       CRC_16_CCITT_FALSE, CRC_16_CDMA2000, CRC_16_DDS_110,
       CRC_16_DECT_R, CRC_16_EN_13757, CRC_16_GENIBUS, CRC_16_MAXIM,
       CRC_16_RIELLO, CRC_16_TELEDISK, CRC_16_USB, CRC_16_CRC_A,
       CRC_16_KERMIT, CRC_16_MODBUS, CRC_16_X_25, CRC_16_XMODEM

import Base: start, done, next


function check_generator{G<:Unsigned}(degree::Int, generator::G, chunk_size::Int)
    @assert degree <= 8 * sizeof(G) "generator too small for degree"
    # the shift needed to move something of chunk_size to the msb-1
    shift = degree - chunk_size
    @assert shift >= 0 "polynomial smaller than data chunk"
    # this is carry before shift, so is (implicit) msb-1
    carry = one(G) << (degree - 1)
    rem_mask = convert(G, (1 << degree) - 1)
    (generator & rem_mask, shift, carry, rem_mask)
end


# basic calculation without a table

# we are careful to allow a missing msb in the generator, since that allows
# 8th degree polynomials to be specified in 8 bits, etc.

function rem_no_table{D<:Unsigned, G<:Unsigned}(::Type{D}, degree::Int, generator::G, data; init=nothing)
    word_size = 8 * sizeof(D)
    generator, shift, carry, rem_mask = check_generator(degree, generator, word_size)
    remainder::G = init == nothing ? zero(G) : init
    for word in data
        remainder = remainder $ (convert(G, word) << shift)
        for _ in 1:word_size
            if remainder & carry == carry
                remainder = (remainder << 1) $ generator
            else
                remainder <<= 1
            end
        end
    end
    # when the generator is smaller than the data type we don't lose
    # bits by overflow, so trim before returning
    remainder & rem_mask
end

rem_no_table{D<:Unsigned, G<:Unsigned}(degree::Int, generator::G, data::Vector{D}; init=nothing) = rem_no_table(D, degree, generator, data, init=init)


# generate a lookup table of the requested size

function make_table{G<:Unsigned}(degree::Int, generator::G, table_size::Int)
    @assert table_size < 33 "table too large"  # even this is huge
    generator, shift, carry, rem_mask = check_generator(degree, generator, table_size)
    size = 2 ^ table_size
    table = Array(G, size)
    for word in 0:(size-1)
        remainder::G = convert(G, word << shift)
        for _ in 1:table_size
            if remainder & carry == carry
                remainder = (remainder << 1) $ generator
            else
                remainder = remainder << 1
            end
        end
        table[word+1] = remainder
    end
    table
end


# use a table that matches the size of the input data words.

function rem_word_table{D<:Unsigned, G<:Unsigned}(::Type{D}, degree::Int, generator::G, data, table::Vector{G}; init=nothing)
    word_size = 8 * sizeof(D)
    @assert 2 ^ word_size == length(table) "wrong sized table"
    generator, shift, carry, rem_mask = check_generator(degree, generator, word_size)
    remainder::G = init == nothing ? zero(G) : init
    for word::D in data
        remainder = remainder $ (convert(G, word) << shift)
        remainder = rem_mask & ((remainder << word_size) $ table[1 + (remainder >>> shift)])
    end
    remainder
end

rem_word_table{D<:Unsigned, G<:Unsigned}(degree::Int, generator::G, data::Vector{D}, table::Vector{G}; init=nothing) = rem_word_table(D, degree, generator, data, table, init=init)


# use a table that is smaller than the size of the input data words
# (for efficiency it must be an exact divisor).

function rem_small_table{D<:Unsigned, G<:Unsigned}(::Type{D}, degree::Int, generator::G, data, table::Vector{G}; init=nothing)
    word_size = 8 * sizeof(D)
    block_size = iround(log2(length(table)))
    @assert word_size >= block_size "table too large for input words"
    @assert word_size % block_size == 0 "table block size is not an exact divisor of input word size"
    generator, word_shift, carry, rem_mask = check_generator(degree, generator, word_size)
    n_shifts = div(word_size, block_size)
    block_shift = degree - block_size
    block_mask = convert(G, (1 << block_size) - 1) << block_shift
    remainder::G = init == nothing ? zero(G) : init
    for word::D in data
        tmp = convert(G, word) << word_shift
        for _ in 1:n_shifts
            remainder = remainder $ (tmp & block_mask)
            remainder = rem_mask & ((remainder << block_size) $ table[1 + (remainder >>> block_shift)])
            tmp <<= block_size
        end
    end
    remainder
end

rem_small_table{D<:Unsigned, G<:Unsigned}(degree::Int, generator::G, data::Vector{D}, table::Vector{G}; init=nothing) = rem_small_table(D, degree, generator, data, table,init=init)


# use a table that is larger than the size of the input data words
# (for efficiency it must be an exact multiple).

function rem_big_table{D<:Unsigned, G<:Unsigned}(::Type{D}, degree::Int, generator::G, data, table::Vector{G}; init=nothing)
    word_size = 8 * sizeof(D)
    block_size = iround(log2(length(table)))
    @assert word_size <= block_size "table too small for input words"
    @assert block_size % word_size == 0 "table block size is not an exact multiple of input word size"
    @assert block_size <= degree "table block size is too large for polynomial degree"
    generator, word_shift, carry, rem_mask = check_generator(degree, generator, word_size)
    n_shifts = div(block_size, word_size)
    block_shift = degree - block_size
    remainder::G = init == nothing ? zero(G) : init
    iter = start(data)
    left_shift, right_shift = block_size, block_shift
    while !done(data, iter)
        for i in 1:n_shifts
            if !done(data, iter)
                shift = word_shift - (i-1) * word_size
                word::D, iter = next(data, iter)
                remainder = remainder $ (convert(G, word) << shift)
            else
                left_shift -= word_size
                right_shift += word_size
            end
        end
        remainder = rem_mask & ((remainder << left_shift) $ table[1 + (remainder >>> right_shift)])
    end
    remainder
end

rem_big_table{D<:Unsigned, G<:Unsigned}(degree::Int, generator::G, data::Vector{D}, table::Vector{G}; init=nothing) = rem_big_table(D, degree, generator, data, table, init=init)


# http://stackoverflow.com/questions/2602823/in-c-c-whats-the-simplest-way-to-reverse-the-order-of-bits-in-a-byte
function reflect_bits(n::Uint8)
    n = (n & 0xf0) >> 4 | (n & 0x0f) << 4;
    n = (n & 0xcc) >> 2 | (n & 0x33) << 2;
    (n & 0xaa) >> 1 | (n & 0x55) << 1;
end

REFLECT_8 = Uint8[reflect_bits(i) for i in 0x00:0xff]

reflect(u::Uint8) = REFLECT_8[u+1]

for (T,S) in ((Uint16, Uint8), (Uint32, Uint16), (Uint64, Uint32))
    n = 8 * sizeof(S)
    mask::S = -one(S)
    @eval reflect(u::$T) = (convert($T, reflect(convert($S, u & $mask))) << $n) | reflect(convert($S, (u >>> $n) & $mask))
end


# automatically reflect individual words on iterations

type ReflectWords{T}
    inner::T
end

start{T}(r::ReflectWords{T}) = start(r.inner)
done{T}(r::ReflectWords{T}, state) = done(r.inner, state)

function next{T}(r::ReflectWords{T}, state)
    i, state = next(r.inner, state)
    reflect(i), state
end


TEST = b"123456789"

DEFAULT_BLOCK_SIZE = 16


type Std{G<:Unsigned}

    width::Int    # polynomial degree
    poly::G       # generating poly with msb missing
    init::G       # initial remainder
    refin::Bool   # reflect input
    refout::Bool  # reflect output
    xorout::G     # xored with final remainder
    test::G       # checksum for TEST

    block_size::Int  # number of bits used to index the lookup table
    table::Vector{G}

    # this leaves the table undefined, to be created on first use
    Std(width::Int, poly::G, init::G, refin::Bool, refout::Bool, xorout::G, test::G, block_size::Int) = new(8*sizeof(G), poly, init, refin, refout, xorout, test, block_size)
    
end

# default width (from polynomial type), default table (block) size
Std{G<:Unsigned}(poly::G, init::G, refin::Bool, refout::Bool, xorout::G, test::G) = Std{G}(8*sizeof(G), poly, init, refin, refout, xorout, test, DEFAULT_BLOCK_SIZE)


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



function crc{D<:Unsigned, G<:Unsigned}(::Type{D}, std::Std{G}, data)
    degree = 8*sizeof(G)
    if !isdefined(std, :table)
        std.table = make_table(degree, std.poly, std.block_size)
    end
    if std.refin
        data = ReflectWords(data)
    end
    if sizeof(D) == std.block_size
        remainder = rem_word_table(D, degree, std.poly, data, std.table; init=std.init)
    elseif sizeof(D) > std.block_size
        remainder = rem_small_table(D, degree, std.poly, data, std.table; init=std.init)
    else
        remainder = rem_big_table(D, degree, std.poly, data, std.table; init=std.init)
    end
    if std.refout
        remainder = reflect(remainder)
    end
    remainder $ std.xorout
end

crc{D<:Unsigned, G<:Unsigned}(std::Std{G}, data::Vector{D}) = crc(D, std, data)


end
