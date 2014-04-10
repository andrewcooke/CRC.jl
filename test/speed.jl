
# the core loop for several CRC calculations.  it is used by CRC32,
# whose zlib implementation in C runs ~2.5 times faster than this
# code.

function generic_loop{D<:Unsigned, A<:Unsigned
                      }(::Type{D}, remainder::A, data, table::Vector{A}, word_size)
    for word::D in data
        word $= convert(D, remainder)
        remainder = (remainder >>> word_size) $ table[1 + word]
    end
    remainder
end


# try accessing the lowest byte directly to avoid the shift (slow)

immutable Bytes
    a::Uint8
    b::Uint8
    c::Uint8
    d::Uint8
end

function struct_loop(::Type{Uint8}, remainder::Uint64, data::Vector{Uint8}, table::Vector{Uint64}, ignored)
    r::Uint64 = remainder
    b = reinterpret(Bytes, r)
    for i in 1:length(data)
        @inbounds word::Uint8 = data[i]
        word $= b.a
        r = (r >>> 8) $ table[1 + word]
        b = reinterpret(Bytes, r)
    end
    return r
end


# try accessing via aliased arrays

function array_loop(::Type{Uint8}, remainder::Uint64, data::Vector{Uint8}, table::Vector{Uint64}, ignored)
    r = Array(Uint64, 1)
    @inbounds r[1] = remainder
    b = reinterpret(Uint8, r)
    for i in 1:length(data)
        @inbounds word::Uint8 = data[i]
        @inbounds word $= b[1]
        @inbounds r[1] = (r[1] >>> 8) $ table[1 + word]  # @inbounds no help here
    end
    r[1]
end


# according to my notes this was slightly (~15%) faster than
# generic_loop.  repeated here, however, it seems to be identical to
# the generic case.

function byte_loop{A<:Unsigned
                   }(::Type{Uint8}, remainder::A, data::Vector{Uint8}, table::Vector{A}, ignored)
    for i in 1:length(data)
        @inbounds word::Uint8 = data[i]
        word $= convert(Uint8, remainder)
        remainder = (remainder >>> 8) $ table[1 + word]  # @inbounds no help here
    end
    remainder
end



# timing with a standard data set (the table is "meaningless")

srand(0)  # repeatable results
TABLE = Uint64[rand(Uint64) for _ in 1:256]
DATA = Uint8[rand(Uint8) for _ in 1:10_000_000]
#KNOWN = 17880998858342525629  # 1 million
KNOWN = 3923249127266084647  # 10 million
#KNOWN = 10790464939549839568  # 100 million

function run_std(loop)
    loop(Uint8, zero(Uint64), DATA[1:10], TABLE, 8)  # warmup
    @printf("%15s: ", loop)
    @time (result = loop(Uint8, zero(Uint64), DATA, TABLE, 8))
    @assert result == KNOWN
end

for loop in (generic_loop, struct_loop, byte_loop, array_loop)
    run_std(loop)
end

#  generic_loop: elapsed time: 0.023491408 seconds (16 bytes allocated)
#   struct_loop: elapsed time: 2.027252231 seconds (1680000168 bytes allocated)
#     byte_loop: elapsed time: 0.02304929 seconds (16 bytes allocated)
#    array_loop: elapsed time: 0.045178428 seconds (168 bytes allocated)
