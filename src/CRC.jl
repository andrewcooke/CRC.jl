
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
       rem_big_table, Std, crc, Reverse, reverse,
       TEST, CCITT, CCITT_1D0F, XMODEM, KERMIT

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
function reverse_bits(n::Uint8)
    n = (n & 0xf0) >> 4 | (n & 0x0f) << 4;
    n = (n & 0xcc) >> 2 | (n & 0x33) << 2;
    (n & 0xaa) >> 1 | (n & 0x55) << 1;
end

REVERSED_8 = Uint8[reverse_bits(i) for i in 0x00:0xff]

# other types can define reverse too
reverse(u::Uint8) = REVERSED_8[u+1]

# automatically apply revverse() on iteration

type Reverse{T}
    inner::T
end

start{T}(r::Reverse{T}) = start(r.inner)
done{T}(r::Reverse{T}, state) = done(r.inner, state)

function next{T}(r::Reverse{T}, state)
    i, state = next(r.inner, state)
    reverse(i), state
end


TEST = b"123456789"

DEFAULT_BLOCK_SIZE = 16


type Std{G<:Unsigned}

    width::Int    # polynomial degree
    poly::G       # generating poly with msb missing
    init::G       # initial remainder
    refin::Bool   # reverse input
    refout::Bool  # reverse output
    test::G       # checksum for TEST

    block_size::Int  # number of bits used to index the lookup table
    table::Vector{G}

    # this leaves the table undefined, to be created on first use
    Std(width::Int, poly::G, init::G, refin::Bool, refout::Bool, test::G, block_size::Int) = new(8*sizeof(G), poly, init, refin, refout, test, block_size)
    
end

# default width (from polynomial type) and default table (block) size
Std{G<:Unsigned}(poly::G, init::G, refin::Bool, refout::Bool, test::G) = Std{G}(8*sizeof(G), poly, init, refin, refout, test, DEFAULT_BLOCK_SIZE)


# TODO - REDO
# http://reveng.sourceforge.net/crc-catalogue/16.htm#crc.cat.crc-16-ccitt-false

# http://www.zlib.net/crc_v3.txt
# http://stackoverflow.com/questions/1918090/crc-test-vectors-for-crc16-ccitt
CCITT = Std(0x1021, 0xffff, false, false, 0x29b1)
# http://acooke.org/cute/16bitCRCAl0.html
# http://www.lammertbies.nl/comm/info/crc-calculation.html
CCITT_1D0F = Std(0x1021, 0x1d0f, false, false, 0xe5cc)
# http://acooke.org/cute/16bitCRCAl0.html
# the "painless reversed" algorithm is equivalent to reversing and reflecting
# each byte of the poly?
# http://www.lammertbies.nl/comm/info/crc-calculation.html
XMODEM = Std(0x1021, 0x0000, false, false, 0x31c3)
# TODO - is this flip-reversed?!
KERMIT = Std(0x1201, 0x0000, true, true, 0x2189)


function crc{D<:Unsigned, G<:Unsigned}(::Type{D}, std::Std{G}, data)
    degree = 8*sizeof(G)
    if !isdefined(std, :table)
        std.table = make_table(degree, std.poly, std.block_size)
    end
    if std.refin
        data = Reverse(data)
    end
    if sizeof(D) == std.block_size
        remainder = rem_word_table(D, degree, std.poly, data, std.table; init=std.init)
    elseif sizeof(D) > std.block_size
        remainder = rem_small_table(D, degree, std.poly, data, std.table; init=std.init)
    else
        remainder = rem_big_table(D, degree, std.poly, data, std.table; init=std.init)
    end
    remainder
end

crc{D<:Unsigned, G<:Unsigned}(std::Std{G}, data::Vector{D}) = crc(D, std, data)


end
