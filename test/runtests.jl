using CRC
using Base.Test
using IntModN

function test_rem_no_table()
    for _ in 1:100
        a1 = rand(Uint8, 2)
        b1 = rand(Uint8)
        println("a1 $(a1[1]) $(a1[2]) b1 $b1")
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 16 + convert(Uint64, a1[2]) << 8)
        b2 = convert(Uint64, (1 << 8) | b1)
        println("a2 $a2 b2 $b2")
        c1 = rem_no_table(8, b1, a1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        println("c1 $c1 c2 $c2 ($(c2.i))")
        @test c2.i == c1
        @test c1 < b2
        @test c2.i < b2
    end
    println("rem_no_table ok")
end


srand(0)
test_rem_no_table()

