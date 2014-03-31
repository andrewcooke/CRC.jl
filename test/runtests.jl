using CRC
using Base.Test
using IntModN

function test_rem_no_table()
    print("rem_no_table")

    for _ in 1:10

        # an 8th degree generator with 2 bytes of data.  do the check as
        # 64 bit polynpomial division (don't forget zero padding).
        a1 = rand(Uint8, 2)
        b1 = rand(Uint8)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 16 + convert(Uint64, a1[2]) << 8)
        b2 = convert(Uint64, (1 << 8) | b1)
        c1 = rem_no_table(8, b1, a1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

        # using the same data, a 3rd degree generator with 2 bytes of data.
        b1 = convert(Uint8, (1 << 3) | (b1 & ((1 << 3) - 1)))
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1[2]) << 3)
        b2 = convert(Uint64, b1)
        c1 = rem_no_table(3, b1, a1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

        # a 16th degree polynomial with 3 bytes of data (now 16 bits
        # of padding)
        a1 = rand(Uint8, 3)
        b1 = rand(Uint16)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 32 + convert(Uint64, a1[2]) << 24 + convert(Uint64, a1[3]) << 16)
        b2 = convert(Uint64, (1 << 16) | b1)
        c1 = rem_no_table(16, b1, a1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

        # using the same data, a 9th degree polynomial with 3 bytes
        # of data (now 9 bits of padding)
        b1 = convert(Uint16, (1 << 9) | (b1 & ((1 << 9) - 1)))
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 25 + convert(Uint64, a1[2]) << 17 + convert(Uint64, a1[3]) << 9)
        b2 = convert(Uint64, b1)
        c1 = rem_no_table(9, b1, a1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

    end

    println("ok")
end

function test_rem_word_table()
    print("rem_word_table")

    for _ in 1:10

        # an 8th degree generator with 2 bytes of data.  do the check as
        # 64 bit polynpomial division (don't forget zero padding).
        a1 = rand(Uint8, 2)
        b1 = rand(Uint8)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 16 + convert(Uint64, a1[2]) << 8)
        b2 = convert(Uint64, (1 << 8) | b1)
        c1 = rem_word_table(8, b1, a1, make_table(8, b1, 8*sizeof(a1[1])))
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

        # using the same data, a 3rd degree generator with 2 bytes of data.
        b1 = convert(Uint8, (1 << 3) | (b1 & ((1 << 3) - 1)))
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1[2]) << 3)
        b2 = convert(Uint64, b1)
        c1 = rem_word_table(3, b1, a1, make_table(3, b1, 8*sizeof(a1[1])))
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

        # a 16th degree polynomial with 3 bytes of data (now 16 bits
        # of padding)
        a1 = rand(Uint8, 3)
        b1 = rand(Uint16)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 32 + convert(Uint64, a1[2]) << 24 + convert(Uint64, a1[3]) << 16)
        b2 = convert(Uint64, (1 << 16) | b1)
        c1 = rem_word_table(16, b1, a1, make_table(16, b1, 8*sizeof(a1[1])))
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

        # using the same data, a 9th degree polynomial with 3 bytes
        # of data (now 9 bits of padding)
        b1 = convert(Uint16, (1 << 9) | (b1 & ((1 << 9) - 1)))
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 25 + convert(Uint64, a1[2]) << 17 + convert(Uint64, a1[3]) << 9)
        b2 = convert(Uint64, b1)
        c1 = rem_word_table(9, b1, a1, make_table(9, b1, 8*sizeof(a1[1])))
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

    end

    println("ok")
end

function test_rem_small_table()
    print("rem_small_table")

    for _ in 1:10

        # an 8th degree generator with 2 bytes of data.  do the check as
        # 64 bit polynpomial division (don't forget zero padding).
        a1 = rand(Uint8, 2)
        b1 = rand(Uint8)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 16 + convert(Uint64, a1[2]) << 8)
        b2 = convert(Uint64, (1 << 8) | b1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c2.i < b2
        c1 = rem_small_table(8, b1, a1, make_table(8, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_small_table(8, b1, a1, make_table(8, b1, 4*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        print(".")

        # a 16th degree polynomial with 3 bytes of data (now 16 bits
        # of padding)
        a1 = rand(Uint8, 3)
        b1 = rand(Uint16)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 32 + convert(Uint64, a1[2]) << 24 + convert(Uint64, a1[3]) << 16)
        b2 = convert(Uint64, (1 << 16) | b1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c2.i < b2
        c1 = rem_small_table(16, b1, a1, make_table(16, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_small_table(16, b1, a1, make_table(16, b1, 2*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        print(".")

        # using the same data, a 9th degree polynomial with 3 bytes
        # of data (now 9 bits of padding)
        b1 = convert(Uint16, (1 << 9) | (b1 & ((1 << 9) - 1)))
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 25 + convert(Uint64, a1[2]) << 17 + convert(Uint64, a1[3]) << 9)
        b2 = convert(Uint64, b1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c2.i < b2
        c1 = rem_small_table(9, b1, a1, make_table(9, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_small_table(9, b1, a1, make_table(9, b1, 1*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        print(".")

    end

    println("ok")
end


function test_rem_big_table()
    print("rem_big_table")

    for _ in 1:10

        # a 16th degree polynomial with 2 bytes of data (now 16 bits
        # of padding)
        a1 = rand(Uint8, 2)
        b1 = rand(Uint16)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 24 + convert(Uint64, a1[2]) << 16)
        b2 = convert(Uint64, (1 << 16) | b1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c2.i < b2
        c1 = rem_big_table(16, b1, a1, make_table(16, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_big_table(16, b1, a1, make_table(16, b1, 16*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        print(".")

        # a 16th degree polynomial with 3 bytes of data (now 16 bits
        # of padding)
        a1 = rand(Uint8, 3)
        b1 = rand(Uint16)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 32 + convert(Uint64, a1[2]) << 24 + convert(Uint64, a1[3]) << 16)
        b2 = convert(Uint64, (1 << 16) | b1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c2.i < b2
        c1 = rem_big_table(16, b1, a1, make_table(16, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_big_table(16, b1, a1, make_table(16, b1, 16*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        print(".")

    end

    println("ok")
end

function test_reflect()
    print("reflect")
    @test reflect(0b11001010) == 0b01010011
    @test reflect(0x1234) == 0x2c48
    @test reflect(0x12345678) == 0x1e6a2c48
    @test reflect(0x123456789abcdef0) == 0x0f7b3d591e6a2c48
    @test collect(Uint8, ReflectWords([0x12, 0x34])) == [0x48, 0x2c]
    println("ok")
end

function test_tests()
    print("tests")
    for std in (#CRC_3_ROHC,
                CRC_16_ARC, CRC_16_AUG_CCITT, CRC_16_BUYPASS, 
                CRC_16_CCITT_FALSE, CRC_16_CDMA2000, CRC_16_DDS_110,
                CRC_16_DECT_R, CRC_16_EN_13757, CRC_16_GENIBUS, CRC_16_MAXIM,
                CRC_16_RIELLO, CRC_16_TELEDISK, CRC_16_USB, CRC_16_CRC_A,
                CRC_16_KERMIT, CRC_16_MODBUS, CRC_16_X_25, CRC_16_XMODEM)
        if crc(std, TEST) != std.test 
            println("\n$(hex(crc(std, TEST))) $(hex(std.test))")
        end
        @test crc(std, TEST) == std.test 
        print(".")
        # TODO
        @test rem_no_table(std.width, std.poly, TEST, init=std.init)
        print(".")
    end
    println("ok")
end

function time_table_size()
    CCITT_8 = Std{Uint16}(0x1021, 8)
    CCITT_16 = Std{Uint16}(0x1021, 16)
    data = rand(Uint8, 100_000_000)
    @test crc(CITT_8, data) == crc(CITT_16, data)
    @time crc(CITT_8, data)   # 0.46s
    @time crc(CITT_16, data)  # 0.33s  larger tables are faster
end


srand(0)  # repeatable results
test_rem_no_table()
test_rem_word_table()
test_rem_small_table()
test_rem_big_table()
test_reflect()
test_tests()

#time_table_size()

