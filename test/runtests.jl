
using CRC
using Base.Test
#using IntModN
import Zlib: crc32


function test_crc(spec)
    print(spec)
    for tables in (NoTables, Single, Multiple)
        result = crc(spec, tables=tables)(CHECK)
        if result != spec.check
            println("$tables $(hex(result))")
            @test result == spec.check
        end
        print(".")
    end
    println("ok")
end

function test_all()
    print("all")
    bad = Set()
    for _ in 1:10
        data = rand(Uint8, rand(100:200))
        for (name, spec) in ALL
            if ! in(spec, bad)
                r1 = crc(spec, tables=NoTables)(data)
                for tables in (Single, Multiple)
                    r2 = crc(spec, tables=tables)(data)
                    if r1 != r2
                        push!(bad, spec)
                    end
                end
                print(".")
            end
        end
    end
    if length(bad) > 0
        println("failed:")
        for spec in bad
            println(spec)
        end
        @test false
    else
        println("ok")
    end
end

function tests()
    test_crc(CRC_3_ROHC)
    test_crc(CRC_4_ITU)
    test_crc(CRC_7_ROHC)
    test_crc(CRC_32)
    test_crc(CRC_7)
    test_crc(CRC_8)
    test_crc(CRC_10)
    test_all()
end

tests()


function time_libz()

    data = rand(Uint8, 300_000_000)
    check = crc32(data)
    @time crc32(data)
    for tables in (Single, Multiple)
        ours = crc(CRC_32, tables=tables)
        @assert ours(data) == check
        println(tables)
        @time ours(data)
    end
end

function time_padded()
    ours = crc(CRC_64)
    data = rand(Uint8, 300_000_000)
    @assert ours(CHECK) == CRC_64.check
    @time ours(data)
end

function time_no_tables()
    ours = crc(CRC_15, tables=NoTables)
    data = rand(Uint8, 30_000_000)
    @assert ours(CHECK) == CRC_15.check
    @time ours(data)
end

srand(0)  # repeatable results

time_libz()
time_padded()
time_no_tables()
