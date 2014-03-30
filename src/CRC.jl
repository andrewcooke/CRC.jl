
# calculate crc checksums for data.  this process is closely related
# to the routines in IntModN (particularly GF2Poly), but the
# traditional implementations are optimized to the point where
# implementing them from "first principles" makes little sense
# (although we can use IntModN to check the results here).

# the basic idea is to do a polynomial division along the byte stream.
# it's all explained very nicely at
# http://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks

module CRC

export rem_no_table, make_table, rem_word_table

function check_generator{G<:Unsigned}(degree::Int, generator::G, chunk_size::Int)
    @assert degree <= 8 * sizeof(G) "generator too small for degree"
    shift = degree - chunk_size
    @assert shift >= 0 "polynomial smaller than data chunk"
    # this is carry before shift, so is (implicit) msb / 2
    carry = one(G) << (degree - 1)
    mask = convert(G, (1 << degree) - 1)
    (generator & mask, shift, carry, mask)
end


# basic calculation without a table

# we are careful to allow a missing msb in the generator, since that allows
# 8th degree polynomials to be specified in 8 bits, etc.

function rem_no_table{G<:Unsigned,D<:Unsigned}(degree::Int, generator::G, data::Vector{D})
    word_size = 8 * sizeof(D)
    generator, shift, carry, mask = check_generator(degree, generator, word_size)
    remainder::G = zero(G)
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
    remainder & mask
end


# generate a lookup table of the requested size

function make_table{G<:Unsigned}(degree::Int, generator::G, table_size::Int)
    @assert table_size < 33 "table too large"  # even this is huge
    generator, shift, carry, mask = check_generator(degree, generator, table_size)
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

function rem_word_table{G<:Unsigned,D<:Unsigned}(degree::Int, generator::G, data::Vector{D}, table::Vector{G})
    word_size = 8 * sizeof(D)
    @assert 2 ^ word_size == length(table) "wrong sized table"
    generator, shift, carry, mask = check_generator(degree, generator, word_size)
    remainder::G = zero(G)
    for word in data
        remainder = remainder $ (convert(G, word) << shift)
        remainder = mask & ((remainder << word_size) $ table[1 + (remainder >>> shift)])
    end
    remainder
end


end
