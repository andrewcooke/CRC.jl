
using CRC
using Test
using Random
import Libz: crc32

@testset "Consistency between no table and tables" begin
    for _ in 1:10
        data = rand(UInt8, rand(100:200))
        for (name, spec) in ALL
                notable = crc(spec, tables=NoTables)(data)
                singletable = crc(spec, tables=Single)(data)
                multitable = crc(spec, tables=Multiple)(data)
                @test notable == singletable == multitable
        end
    end
end

@testset "Handle string" begin
    crc32 = crc(CRC_32)
    @test crc32("abcxyz") == 0xacc462e9
end

@testset "Handle stream" begin
    crc32 = crc(CRC_32)
    @test crc32(IOBuffer("abcxyz")) == 0xacc462e9
end

@testset "Check against spec: $id" for (id, spec) in ALL
    for tables in (NoTables, Single, Multiple)
        result = crc(spec, tables=tables)(CHECK)
        @test result == spec.check
    end
end

@testset "CRC32 consistency with Libz length $len" for len in (0,1,1000)
    data = rand(UInt8, len)
    check = crc32(data)
    @test crc(CRC_32, tables=Single)(data) == check
    @test crc(CRC_32, tables=Multiple)(data) == check
end
