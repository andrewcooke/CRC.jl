
# calculate crc checksums for data.  this process is closely related
# to the routines in IntModN (particularly GF2Poly), but the
# traditional implementations are optimized to the point where
# implementing them from "first principles" makes little sense
# (although we can use IntModN to check the results here).

module CRC

export rem_no_table

# the basic idea is to do a polynomial division along the byte stream.
# it's all explained very nicely at
# http://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
# - here we implement the "byte at a time" approach, without tables.

# we are careful to allow a missing msb in the generator, since that allows
# 8th degree polynomials to be specified in 8 bits, etc.
function rem_no_table{G<:Unsigned,D<:Unsigned}(degree::Int, generator::G, data::Vector{D})
    mask = convert(G, (1 << degree) - 1)
    # this is carry before shift, so is (implicit) msb / 2
    carry = one(G) << (degree - 1)
    word_size = 8 * sizeof(D)
    shift = degree - word_size
    @assert degree <= 8 * sizeof(G) "generator too small for degree"
    @assert shift >= 0 "polynomial smaller than data chunk"
    remainder::G = zero(G)
    for word in data
        remainder = remainder $ (convert(G, word) << shift)
        for i in 1:word_size
            if remainder & carry == carry
                remainder = (remainder << 1) $ generator
            else
                remainder <<= 1
            end
        end
    end
    # when the generator is smaller than the data type we don't lose
    # bits by overflow, so trim before returning (no need to discard
    # optional explicit msb before loop because this does the same)
    remainder & mask
end


end
