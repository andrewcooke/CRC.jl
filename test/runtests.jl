using CRC
using Base.Test
using IntModN

function test_largest()
    @assert CRC.largest(Uint8, Uint8) == Uint8
    @assert CRC.largest(Uint8, Uint16) == Uint16
    @assert CRC.largest(Uint16, Uint8) == Uint16
end

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
        c1 = rem_word_table(8, b1, a1, make_table(Uint8, 8, b1, 8*sizeof(a1[1])))
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c1 < b2
        @test c2.i < b2
        @test c2.i == c1
        print(".")

        # using the same data, a 3rd degree generator with 2 bytes of data.
        b1 = convert(Uint8, (1 << 3) | (b1 & ((1 << 3) - 1)))
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1[2]) << 3)
        b2 = convert(Uint64, b1)
        c1 = rem_word_table(3, b1, a1, make_table(Uint8, 3, b1, 8*sizeof(a1[1])))
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
        c1 = rem_word_table(16, b1, a1, make_table(Uint16, 16, b1, 8*sizeof(a1[1])))
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
        c1 = rem_word_table(9, b1, a1, make_table(Uint16, 9, b1, 8*sizeof(a1[1])))
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
        c1 = rem_small_table(8, b1, a1, make_table(Uint8, 8, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_small_table(8, b1, a1, make_table(Uint8, 8, b1, 4*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_small_table(8, b1, a1, make_table(Uint16, 8, b1, 4*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        print(".")

        # using the same data, a 3rd degree generator with 2 bytes of data.
        b1 = convert(Uint8, (1 << 3) | (b1 & ((1 << 3) - 1)))
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1[2]) << 3)
        b2 = convert(Uint64, b1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c2.i < b2
        c1 = rem_small_table(3, b1, a1, make_table(Uint8, 3, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_small_table(3, b1, a1, make_table(Uint16, 3, b1, 4*sizeof(a1[1])))
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
        c1 = rem_small_table(16, b1, a1, make_table(Uint16, 16, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_small_table(16, b1, a1, make_table(Uint64, 16, b1, 2*sizeof(a1[1])))
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
        c1 = rem_small_table(9, b1, a1, make_table(Uint64, 9, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_small_table(9, b1, a1, make_table(Uint16, 9, b1, 1*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        print(".")

    end

    println("ok")
end


function test_rem_large_table()
    print("rem_large_table")

    for _ in 1:10

        # a 16th degree polynomial with 2 bytes of data (now 16 bits
        # of padding)
        a1 = rand(Uint8, 2)
        b1 = rand(Uint16)
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 24 + convert(Uint64, a1[2]) << 16)
        b2 = convert(Uint64, (1 << 16) | b1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c2.i < b2
        c1 = rem_large_table(16, b1, a1, make_table(Uint16, 16, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_large_table(16, b1, a1, make_table(Uint64, 16, b1, 16*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        print(".")

        # using the same data, a 3rd degree generator with 2 bytes of data.
        # but use 16 bits so we can use a 16 bit table
        b1 = convert(Uint16, (1 << 3) | (b1 & ((1 << 3) - 1)))
        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1[2]) << 3)
        b2 = convert(Uint64, b1)
        c2 = GF2Poly(a2) % GF2Poly(b2)
        @test c2.i < b2
        c1 = rem_large_table(3, b1, a1, make_table(Uint8, 3, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_large_table(3, b1, a1, make_table(Uint16, 3, b1, 16*sizeof(a1[1])))
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
        c1 = rem_large_table(16, b1, a1, make_table(Uint32, 16, b1, 8*sizeof(a1[1])))
        @test c1 < b2
        @test c2.i == c1
        c1 = rem_large_table(16, b1, a1, make_table(Uint64, 16, b1, 16*sizeof(a1[1])))
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
    for std in (:CRC_3_ROHC, :CRC_4_ITU, :CRC_5_EPC, :CRC_5_ITU,
                :CRC_5_USB, :CRC_6_CDMA2000_A, :CRC_6_CDMA2000_B,
                :CRC_6_DARC, :CRC_6_ITU, :CRC_7, :CRC_7_ROHC, :CRC_8,
                :CRC_8_CDMA2000, :CRC_8_DARC, :CRC_8_DVB_S2,
                :CRC_8_EBU, :CRC_8_I_CODE, :CRC_8_ITU, :CRC_8_MAXIM,
                :CRC_8_ROHC, :CRC_8_WCDMA, :CRC_10,
                :CRC_10_CDMA2000, :CRC_11, :CRC_12_3GPP,
                :CRC_12_CDMA2000, :CRC_12_DECT, :CRC_13_BBC,
                :CRC_14_DARC, :CRC_15, :CRC_15_MPT1327, :CRC_16_ARC,
                :CRC_16_AUG_CCITT, :CRC_16_BUYPASS,
                :CRC_16_CCITT_FALSE, :CRC_16_CDMA2000,
                :CRC_16_DDS_110, :CRC_16_DECT_R, :CRC_16_EN_13757,
                :CRC_16_GENIBUS, :CRC_16_MAXIM, :CRC_16_RIELLO,
                :CRC_16_TELEDISK, :CRC_16_USB, :CRC_16_CRC_A,
                :CRC_16_KERMIT, :CRC_16_MODBUS, :CRC_16_X_25,
                :CRC_16_XMODEM)

        s = eval(std)
        remainder = crc(s, TEST)
        if remainder != s.test 
            println("\n$std $(hex(remainder)) $(hex(s.test))")
        end
        @test remainder == s.test 
        print(".")

        data = s.refin ? ReflectWords(TEST) : TEST
        remainder = rem_no_table(Uint8, Uint64, s.width, s.poly, data, init=s.init)
        remainder = s.refout ? reflect(s.width, remainder) : remainder
        remainder $= s.xorout
        if remainder != s.test 
            println("\n$std $(hex(remainder)) $(hex(s.test)) (no table)")
        end
        @test remainder == s.test 
        print(".")

    end
    println("ok")
end

function time_table_size()
    CCITT_8_4 = Std{Uint64, Uint16}(16, 0x1021, 0x0000, false, false, 0x0000, 0x0000, 4)
    CCITT_8_8 = Std{Uint64, Uint16}(16, 0x1021, 0x0000, false, false, 0x0000, 0x0000, 8)
    CCITT_64_8 = Std{Uint64, Uint16}(16, 0x1021, 0x0000, false, false, 0x0000, 0x0000, 8)
    CCITT_16_16 = Std{Uint64, Uint16}(16, 0x1021, 0x0000, false, false, 0x0000, 0x0000, 16)
    CCITT_64_16 = Std{Uint64, Uint16}(16, 0x1021, 0x0000, false, false, 0x0000, 0x0000, 16)
    data = rand(Uint8, 100_000_000)
    @test crc(CCITT_8_8, data) == crc(CCITT_8_4, data)
    @test crc(CCITT_8_8, data) == crc(CCITT_16_16, data)
    @test crc(CCITT_64_8, data) == crc(CCITT_64_16, data)

    @time crc(CCITT_8_4, data)   # 1.1
    @time crc(CCITT_8_8, data)   # 0.35
    @time crc(CCITT_64_8, data)  # 0.35
    @time crc(CCITT_16_16, data) # 0.52
    @time crc(CCITT_64_16, data) # 0.52
end


srand(0)  # repeatable results
test_largest()
test_rem_no_table()
test_rem_word_table()
test_rem_small_table()
test_rem_large_table()
test_reflect()
test_tests()

#time_table_size()
