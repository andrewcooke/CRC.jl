
# the core loop for several CRC calculations.  it is used by CRC32,
# whose zlib implementation in C runs ~2.5 times faster than this
# code.

function generic_loop(::Type{D}, remainder::A, data,
        table::Vector{A}, word_size) where {D<:Unsigned, A<:Unsigned}
    for word::D in data
        word ⊻= remainder%D
        remainder = (remainder >>> word_size) ⊻ table[1 + word]
    end
    remainder
end


# try accessing the lowest byte directly to avoid the shift (slow)
struct Bytes
    a::UInt8
    b::UInt8
    c::UInt8
    d::UInt8
end

function struct_loop(::Type{UInt8}, remainder::UInt64, data::Vector{UInt8}, table::Vector{UInt64}, ignored)
    r::UInt64 = remainder
    b = reinterpret(Bytes, r)
    for i in 1:length(data)
        @inbounds word::UInt8 = data[i]
        word ⊻= b.a
        r = (r >>> 8) ⊻ table[1 + word]
        b = reinterpret(Bytes, r)
    end
    return r
end


# try accessing via aliased arrays

function array_loop(::Type{UInt8}, remainder::UInt64, data::Vector{UInt8}, table::Vector{UInt64}, ignored)
    r = Vector{UInt64}(undef, 1)
    @inbounds r[1] = remainder
    b = reinterpret(UInt8, r)
    for i in 1:length(data)
        @inbounds word::UInt8 = data[i]
        @inbounds word ⊻= b[1]
        @inbounds r[1] = (r[1] >>> 8) ⊻ table[1 + word]  # @inbounds no help here
    end
    r[1]
end


# according to my notes this was slightly (~15%) faster than
# generic_loop.  repeated here, however, it seems to be identical to
# the generic case.

function byte_loop(::Type{UInt8}, remainder::A, data::Vector{UInt8}, table::Vector{A}, ignored) where {A<:Unsigned}
    for i in 1:length(data)
        word = data[i]
        word ⊻= remainder%UInt8
        remainder = (remainder >>> 8) ⊻ table[1 + word]
    end
    remainder
end



# timing with a standard data set (the table is "meaningless")

Random.seed!(0)  # repeatable results
TABLE = UInt64[rand(UInt64) for _ in 1:256]
DATA = UInt8[rand(UInt8) for _ in 1:10_000_000]
#KNOWN = 17880998858342525629  # 1 million
KNOWN = 0x65280cfa186b00d5  # 10 million
# KNOWN = 10790464939549839568  # 100 million
using Printf
function run_std(loop)
    loop(UInt8, zero(UInt64), DATA[1:10], TABLE, 8)  # warmup
    @printf("%15s: ", loop)
    @time (result = loop(UInt8, zero(UInt64), DATA, TABLE, 8))
    @assert result == KNOWN
end

for loop in (generic_loop, byte_loop, array_loop)
    run_std(loop)
end
run_std(array_loop)

#  generic_loop: elapsed time: 0.023491408 seconds (16 bytes allocated)
#   struct_loop: elapsed time: 2.027252231 seconds (1680000168 bytes allocated)
#     byte_loop: elapsed time: 0.02304929 seconds (16 bytes allocated)
#    array_loop: elapsed time: 0.045178428 seconds (168 bytes allocated)

# benchmarks against Libz

SIZE = 300_000_000
# SIZE = 300_000

function time_libz()
    println("libz")
    data = rand(UInt8, SIZE)
    check = crc32(data)
    @time crc32(data)
    for tables in (Single, Multiple)
        ours = crc(CRC_32, tables=tables)
        @assert ours(data) == check
        println(tables)
        @time ours(data)
    end
end

function time_no_tables()
    println("no_tables")
    ours = crc(CRC_15, tables=NoTables)
    data = rand(UInt8, round(Int, SIZE//10))
    @assert ours(CHECK) == CRC_15.check
    @time ours(data)
end

function time_padded()
    println("padded")
    ours = crc(CRC_64)
    data = rand(UInt8, SIZE)
    @assert ours(CHECK) == CRC_64.check
    @time ours(data)
end

Random.seed!(0)  # repeatable results

time_libz()
time_no_tables()
time_padded()
