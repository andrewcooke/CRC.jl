
using CRC
using Base.Test
#using IntModN
import Zlib: crc32

ALL = [CRC_3_ROHC, CRC_4_ITU, CRC_5_EPC, CRC_5_ITU,
       CRC_5_USB, CRC_6_CDMA2000_A, CRC_6_CDMA2000_B, CRC_6_DARC,
       CRC_6_ITU, CRC_7, CRC_7_ROHC, CRC_8, CRC_8_CDMA2000,
       CRC_8_DARC, CRC_8_DVB_S2, CRC_8_EBU, CRC_8_I_CODE, CRC_8_ITU,
       CRC_8_MAXIM, CRC_8_ROHC, CRC_8_WCDMA, CRC_10, CRC_10_CDMA2000,
       CRC_11, CRC_12_3GPP, CRC_12_CDMA2000, CRC_12_DECT, CRC_13_BBC,
       CRC_14_DARC, CRC_15, CRC_15_MPT1327, CRC_16_ARC,
       CRC_16_AUG_CCITT, CRC_16_BUYPASS, CRC_16_CCITT_FALSE,
       CRC_16_CDMA2000, CRC_16_DDS_110, CRC_16_DECT_R, CRC_16_DECT_X,
       CRC_16_DNP, CRC_16_EN_13757, CRC_16_GENIBUS, CRC_16_MAXIM,
       CRC_16_RIELLO, CRC_16_TELEDISK, CRC_16_USB, CRC_16_CRC_A,
       CRC_16_KERMIT, CRC_16_MODBUS, CRC_16_X_25, CRC_16_XMODEM,
       CRC_24, CRC_24_FLEXRAY_A, CRC_24_FLEXRAY_B, CRC_31_PHILIPS,
       CRC_32, CRC_32_BZIP2, CRC_32_C, CRC_32_D, CRC_32_MPEG_2,
       CRC_32_POSIX, CRC_32_Q, CRC_32_JAMCRC, CRC_32_XFER, CRC_40_GSM,
       CRC_64, CRC_64_WE, CRC_64_XZ, CRC_82_DARC]

function test_crc(spec)
    print(spec)
    @test crc(spec, TEST) == spec.test
    print(".")
    result = crc(spec, Uint8, false)(TEST)
    if result != spec.test
        println("false $(hex(result))")
        @test result == spec.test
    end
    print(".")
    result= crc(spec, Uint8, true)(TEST)
    if result != spec.test
        println("true $(hex(result))")
        @test result == spec.test
    end
    println("ok")
end

function test_crc_no_table()
    print("no table")
    for spec in ALL
        result = crc(spec, TEST)
        if result != spec.test
            println("$spec $(hex(result)) $(hex(spec.test))")
            @test result == spec.test
        end
        print(".")
    end
    println("ok")
end

function test_tables()
    print("tables")
    tables = make_tables(Uint32, Uint32, CRC_32.width, CRC_32.poly, CRC_32.refin)
    # values from zlib crc32.h
    @test length(tables) == 4
    for t in 1:4
        @test tables[t][1] == 0x00000000
        print(".")
    end
    @test tables[1][2] == 0x77073096
    @test tables[1][256] == 0x2d02ef8d
    @test tables[2][2] == 0x191b3141
    @test tables[4][256] == 0xde0506f1
    println("ok")
end

function test_crc_table(flag)
    print("table $flag")
    for spec in ALL
        c = crc(spec, Uint8, flag)
        result = c(TEST)
        if result != spec.test
            println("$spec $(hex(result)) $(hex(spec.test))")
            @test result == spec.test
        end
        print(".")
    end
    println("ok")
end

function test_all()
    bad = Set()
    for _ in 1:10
        data = rand(Uint8, rand(100:200))
#        data = rand(Uint8, rand(10:20))
        for spec in ALL
            if ! in(spec, bad)
#                r1 = crc(spec, data)
                r1 = crc(spec, Uint8, false)(data)
                r2 = crc(spec, Uint8, true)(data)
                if r1 != r2
                    push!(bad, spec)
                end
            end
        end
    end
    if length(bad) > 0
        println("failed:")
        for spec in bad
            println(spec)
        end
        @test false
    end
end

function tests()
    test_crc(CRC_3_ROHC)
    test_crc(CRC_7_ROHC)
    test_crc(CRC_4_ITU)
    test_crc(CRC_32)
    test_crc(CRC_7)
    test_crc(CRC_8)
    test_crc(CRC_10)
#    test_crc(CRC_8_CDMA2000)
#    test_crc(CRC_5_EPC)
    test_crc_no_table()
    test_tables()
    test_crc_table(false)
#    test_crc_table(true)
    test_all()
end

tests()


#function test_largest()
#    @assert CRC.largest(Uint8, Uint8) == Uint8
#    @assert CRC.largest(Uint8, Uint16) == Uint16
#    @assert CRC.largest(Uint16, Uint8) == Uint16
#end
#
#function test_crc_no_table()
#    print("crc_no_table")
#
#    for _ in 1:10
#
#        # an 8th degree generator with 2 bytes of data.  do the check as
#        # 64 bit polynpomial division (don't forget zero padding).
#        a1 = rand(Uint8, 2)
#        b1 = rand(Uint8)
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 16 + convert(Uint64, a1#[2]) << 8)
#        b2 = convert(Uint64, (1 << 8) | b1)
#        c1 = crc_no_table(8, b1, a1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c1 < b2
#        @test c2.i < b2
#        @test c2.i == c1
#        print(".")
#
#        # now swap the bytes and use refin
#        a1r = collect(Uint8, map(reflect, a1))
#        c1r = crc_no_table(8, b1, a1r, refin=true)        
#        @test c1r < b2
#        @test c2.i == c1r
#        print(".")
#
#        # using the same data, a 3rd degree generator with 2 bytes of data.
#        b1 = convert(Uint8, (1 << 3) | (b1 & ((1 << 3) - 1)))
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1#[2]) << 3)
#        b2 = convert(Uint64, b1)
#        c1 = crc_no_table(3, b1, a1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c1 < b2
#        @test c2.i < b2
#        @test c2.i == c1
#        print(".")
#
#        # a 16th degree polynomial with 3 bytes of data (now 16 bits
#        # of padding)
#        a1 = rand(Uint8, 3)
#        b1 = rand(Uint16)
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 32 + convert(Uint64, a1#[2]) << 24 + convert(Uint64, a1[3]) << 16)
#        b2 = convert(Uint64, (1 << 16) | b1)
#        c1 = crc_no_table(16, b1, a1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c1 < b2
#        @test c2.i < b2
#        @test c2.i == c1
#        print(".")
#
#        # using the same data, a 9th degree polynomial with 3 bytes
#        # of data (now 9 bits of padding)
#        b1 = convert(Uint16, (1 << 9) | (b1 & ((1 << 9) - 1)))
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 25 + convert(Uint64, a1#[2]) << 17 + convert(Uint64, a1[3]) << 9)
#        b2 = convert(Uint64, b1)
#        c1 = crc_no_table(9, b1, a1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c1 < b2
#        @test c2.i < b2
#        @test c2.i == c1
#        print(".")
#
#    end
#
#    println("ok")
#end
#
#function test_crc_word_table()
#    print("crc_word_table")
#
#    for _ in 1:10
#
#        # an 8th degree generator with 2 bytes of data.  do the check as
#        # 64 bit polynpomial division (don't forget zero padding).
#        a1 = rand(Uint8, 2)
#        b1 = rand(Uint8)
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 16 + convert(Uint64, a1#[2]) << 8)
#        b2 = convert(Uint64, (1 << 8) | b1)
#        c1 = crc_word_table(8, b1, a1, make_table(Uint8, 8, b1, 8*sizeof(a1[1]#)))
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c1 < b2
#        @test c2.i < b2
#        @test c2.i == c1
#        print(".")
#
#        # now swap the bytes and use refin
#        a1r = collect(Uint8, map(reflect, a1))
#        c1r = crc_word_table(8, b1, a1r, make_table(Uint8, 8, b1, 8*sizeof(a1r#[1]), refin=true), refin=true)
#        @test c1r < b2
#        @test c2.i == c1r
#        print(".")
#
#        # using the same data, a 3rd degree generator with 2 bytes of data.
#        b1 = convert(Uint8, (1 << 3) | (b1 & ((1 << 3) - 1)))
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1#[2]) << 3)
#        b2 = convert(Uint64, b1)
#        c1 = crc_word_table(3, b1, a1, make_table(Uint8, 3, b1, 8*sizeof(a1[1]#)))
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c1 < b2
#        @test c2.i < b2
#        @test c2.i == c1
#        print(".")
#
#        # a 16th degree polynomial with 3 bytes of data (now 16 bits
#        # of padding)
#        a1 = rand(Uint8, 3)
#        b1 = rand(Uint16)
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 32 + convert(Uint64, a1#[2]) << 24 + convert(Uint64, a1[3]) << 16)
#        b2 = convert(Uint64, (1 << 16) | b1)
#        c1 = crc_word_table(16, b1, a1, make_table(Uint16, 16, b1, 8*sizeof(a1#[1])))
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c1 < b2
#        @test c2.i < b2
#        @test c2.i == c1
#        print(".")
#
#        # using the same data, a 9th degree polynomial with 3 bytes
#        # of data (now 9 bits of padding)
#        b1 = convert(Uint16, (1 << 9) | (b1 & ((1 << 9) - 1)))
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 25 + convert(Uint64, a1#[2]) << 17 + convert(Uint64, a1[3]) << 9)
#        b2 = convert(Uint64, b1)
#        c1 = crc_word_table(9, b1, a1, make_table(Uint16, 9, b1, 8*sizeof(a1[1#])))
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c1 < b2
#        @test c2.i < b2
#        @test c2.i == c1
#        print(".")
#
#    end
#
#    println("ok")
#end
#
#function test_crc_small_table()
#    print("crc_small_table")
#
#    for _ in 1:10
#
#        # an 8th degree generator with 2 bytes of data.  do the check as
#        # 64 bit polynpomial division (don't forget zero padding).
#        a1 = rand(Uint8, 2)
#        b1 = rand(Uint8)
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 16 + convert(Uint64, a1#[2]) << 8)
#        b2 = convert(Uint64, (1 << 8) | b1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c2.i < b2
#        c1 = crc_small_table(8, b1, a1, make_table(Uint8, 8, b1, 8*sizeof(a1[1#])))
#        @test c1 < b2
#        @test c2.i == c1
#        c1 = crc_small_table(8, b1, a1, make_table(Uint8, 8, b1, 4*sizeof(a1[1#])))
#        @test c1 < b2
#        @test c2.i == c1
#        c1 = crc_small_table(8, b1, a1, make_table(Uint16, 8, b1, 4*sizeof(a1[#1])))
#        @test c1 < b2
#        @test c2.i == c1
#        print(".")
#
#        # now swap the bytes and use refin
#        a1r = collect(Uint8, map(reflect, a1))
#        c1r = crc_small_table(8, b1, a1r, make_table(Uint8, 8, b1, 8*sizeof(a1#r[1]), refin=true), refin=true)
#        @test c1r < b2
#        @test c2.i == c1r
#        c1r = crc_small_table(8, b1, a1r, make_table(Uint8, 8, b1, 4*sizeof(a1#r[1]), refin=true), refin=true)
#        @test c1r < b2
#        @test c2.i == c1r
#        c1r = crc_small_table(8, b1, a1r, make_table(Uint16, 8, b1, 4*sizeof(a#1r[1]), refin=true), refin=true)
#        @test c1r < b2
#        @test c2.i == c1r
#        print(".")
#
#        # using the same data, a 3rd degree generator with 2 bytes of data.
#        b1 = convert(Uint8, (1 << 3) | (b1 & ((1 << 3) - 1)))
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1#[2]) << 3)
#        b2 = convert(Uint64, b1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c2.i < b2
#        c1 = crc_small_table(3, b1, a1, make_table(Uint8, 3, b1, 8*sizeof(a1[1#])))
#        @test c1 < b2
#        @test c2.i == c1
#        c1 = crc_small_table(3, b1, a1, make_table(Uint16, 3, b1, 4*sizeof(a1[#1])))
#        @test c1 < b2
#        @test c2.i == c1
#        print(".")
#
#        # a 16th degree polynomial with 3 bytes of data (now 16 bits
#        # of padding)
#        a1 = rand(Uint8, 3)
#        b1 = rand(Uint16)
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 32 + convert(Uint64, a1#[2]) << 24 + convert(Uint64, a1[3]) << 16)
#        b2 = convert(Uint64, (1 << 16) | b1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c2.i < b2
#        c1 = crc_small_table(16, b1, a1, make_table(Uint16, 16, b1, 8*sizeof(a#1[1])))
#        @test c1 < b2
#        @test c2.i == c1
#        c1 = crc_small_table(16, b1, a1, make_table(Uint64, 16, b1, 2*sizeof(a#1[1])))
#        @test c1 < b2
#        @test c2.i == c1
#        print(".")
#
#        # using the same data, a 9th degree polynomial with 3 bytes
#        # of data (now 9 bits of padding)
#        b1 = convert(Uint16, (1 << 9) | (b1 & ((1 << 9) - 1)))
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 25 + convert(Uint64, a1#[2]) << 17 + convert(Uint64, a1[3]) << 9)
#        b2 = convert(Uint64, b1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c2.i < b2
#        c1 = crc_small_table(9, b1, a1, make_table(Uint64, 9, b1, 8*sizeof(a1[#1])))
#        @test c1 < b2
#        @test c2.i == c1
#        c1 = crc_small_table(9, b1, a1, make_table(Uint16, 9, b1, 1*sizeof(a1[#1])))
#        @test c1 < b2
#        @test c2.i == c1
#        print(".")
#
#    end
#
#    println("ok")
#end
#
#
#function test_crc_large_table()
#    print("crc_large_table")
#
#    for _ in 1:10
#
#        # a 16th degree polynomial with 2 bytes of data (now 16 bits
#        # of padding)
#        a1 = rand(Uint8, 2)
#        b1 = rand(Uint16)
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 24 + convert(Uint64, a1#[2]) << 16)
#        b2 = convert(Uint64, (1 << 16) | b1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c2.i < b2
#        c1 = crc_large_table(16, b1, a1, make_table(Uint16, 16, b1, 8*sizeof(a#1[1])))
#        @test c1 < b2
#        @test c2.i == c1
#        c1 = crc_large_table(16, b1, a1, make_table(Uint64, 16, b1, 16*sizeof(#a1[1])))
#        @test c1 < b2
#        @test c2.i == c1
#        print(".")
#
#        # now swap the bytes and use refin
#        a1r = collect(Uint8, map(reflect, a1))
#        c1r = crc_large_table(16, b1, a1r, make_table(Uint16, 16, b1, 8*sizeof#(a1r[1]), refin=true), refin=true)
#        @test c1r < b2
#        @test c2.i == c1r
#        c1r = crc_large_table(16, b1, a1r, make_table(Uint64, 16, b1, 16*sizeo#f(a1r[1]), refin=true), refin=true)
#        @test c1r < b2
#        @test c2.i == c1r
#        print(".")
#
#        # using the same data, a 3rd degree generator with 2 bytes of data.
#        # but use 16 bits so we can use a 16 bit table
#        b1 = convert(Uint16, (1 << 3) | (b1 & ((1 << 3) - 1)))
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 11 + convert(Uint64, a1#[2]) << 3)
#        b2 = convert(Uint64, b1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c2.i < b2
#        c1 = crc_large_table(3, b1, a1, make_table(Uint8, 3, b1, 8*sizeof(a1[1#])))
#        @test c1 < b2
#        @test c2.i == c1
#        c1 = crc_large_table(3, b1, a1, make_table(Uint16, 3, b1, 16*sizeof(a1#[1])))
#        @test c1 < b2
#        @test c2.i == c1
#        print(".")
#
#        # a 16th degree polynomial with 3 bytes of data (now 16 bits
#        # of padding)
#        a1 = rand(Uint8, 3)
#        b1 = rand(Uint16)
#        a2 = convert(Uint64, convert(Uint64, a1[1]) << 32 + convert(Uint64, a1#[2]) << 24 + convert(Uint64, a1[3]) << 16)
#        b2 = convert(Uint64, (1 << 16) | b1)
#        c2 = GF2Poly(a2) % GF2Poly(b2)
#        @test c2.i < b2
#        c1 = crc_large_table(16, b1, a1, make_table(Uint32, 16, b1, 8*sizeof(a#1[1])))
#        @test c1 < b2
#        @test c2.i == c1
#        c1 = crc_large_table(16, b1, a1, make_table(Uint64, 16, b1, 16*sizeof(#a1[1])))
#        @test c1 < b2
#        @test c2.i == c1
#        print(".")
#
#    end
#
#    println("ok")
#end
#
#function test_reflect()
#    print("reflect")
#    @test reflect(0b11001010) == 0b01010011
#    @test reflect(0x1234) == 0x2c48
#    @test reflect(0x12345678) == 0x1e6a2c48
#    @test reflect(0x123456789abcdef0) == 0x0f7b3d591e6a2c48
#    @test collect(Uint8, ReflectWords([0x12, 0x34])) == [0x48, 0x2c]
#    println("ok")
#end
#
#function test_tests()
#    print("tests")
#    for std in (:CRC_3_ROHC, :CRC_4_ITU, :CRC_5_EPC, :CRC_5_ITU,
#                :CRC_5_USB, :CRC_6_CDMA2000_A, :CRC_6_CDMA2000_B,
#                :CRC_6_DARC, :CRC_6_ITU, :CRC_7, :CRC_7_ROHC, :CRC_8,
#                :CRC_8_CDMA2000, :CRC_8_DARC, :CRC_8_DVB_S2,
#                :CRC_8_EBU, :CRC_8_I_CODE, :CRC_8_ITU, :CRC_8_MAXIM,
#                :CRC_8_ROHC, :CRC_8_WCDMA, :CRC_10, :CRC_10_CDMA2000,
#                :CRC_11, :CRC_12_3GPP, :CRC_12_CDMA2000, :CRC_12_DECT,
#                :CRC_13_BBC, :CRC_14_DARC, :CRC_15, :CRC_15_MPT1327,
#
#                :CRC_16_ARC, :CRC_16_AUG_CCITT, :CRC_16_BUYPASS,
#                :CRC_16_CCITT_FALSE, :CRC_16_CDMA2000,
#                :CRC_16_DDS_110, :CRC_16_DECT_R, :CRC_16_DECT_X,
#                :CRC_16_DNP, :CRC_16_EN_13757, :CRC_16_GENIBUS,
#                :CRC_16_MAXIM, :CRC_16_RIELLO, :CRC_16_TELEDISK,
#                :CRC_16_USB, :CRC_16_CRC_A, :CRC_16_KERMIT,
#                :CRC_16_MODBUS, :CRC_16_X_25, :CRC_16_XMODEM, 
#
#                :CRC_24, :CRC_24_FLEXRAY_A, :CRC_24_FLEXRAY_B,
#                :CRC_31_PHILIPS, :CRC_32, :CRC_32_BZIP2, :CRC_32_C,
#                :CRC_32_D, :CRC_32_MPEG_2, :CRC_32_POSIX, :CRC_32_Q,
#                :CRC_32_JAMCRC, :CRC_32_XFER, :CRC_40_GSM, :CRC_64,
#                :CRC_64_WE, :CRC_64_XZ, :CRC_82_DARC)
#
#        s = eval(std)
#
#        for A in (Uint8, Uint16, Uint32, Uint64, Uint128)
#
#            if s.width <= 8*sizeof(A)
#                remainder = crc(s, TEST, A, table=false)
#                if remainder != s.test 
#                    println("\n$std $(hex(remainder)) $(hex(s.test))")
#                end
#                @test remainder == s.test 
#                print(".")
#                
#                for index_size in (4, 8, 16)
#                    if index_size <= 8*sizeof(A)
#                        remainder = crc(s, TEST, A, index_size=index_size)
#                        if remainder != s.test 
#                            println("\n$std $(hex(remainder)) $(hex(s.test))")
#                        end
#                        @test remainder == s.test 
#                        print(".")
#                    end
#                end
#            end
#        end
#
#    end
#    println("ok")
#end
#
#function test_types()
#    # want this to give an error in case people are confused about types
#    @test_throws crc(CRC_32, Uint32)(b"abc")
#    # but allow empty
#    crc(CRC_32, Uint32)(b"")
#end
#
#
#function time_libz()
#
#    # this assumes data are pre-reflected
#    C_8_32 = crc(CRC_32, Uint8, Uint32, index_size=8)
#    C_16_32 = crc(CRC_32, Uint8, Uint32, index_size=16)
#    C_8_64 = crc(CRC_32, Uint8, Uint64, index_size=8)
#    C_16_64 = crc(CRC_32, Uint8, Uint64, index_size=16)
#
#    data = rand(Uint8, 300_000_000)
#    check = crc32(data)
#    @assert C_8_32(data) == check
#    @assert C_16_32(data) == check
#    @assert C_8_64(data) == check
#    @assert C_16_64(data) == check
#
#    @time crc32(data)               # 0.09
#    @time C_8_32(data)
#    @time C_16_32(data)
#    @time C_8_64(data)
#    @time C_16_64(data)
#end
#
#
#srand(0)  # repeatable results
#
#function tests()
#    test_largest()
#    test_crc_no_table()
#    test_crc_word_table()
#    test_crc_small_table()
#    test_crc_large_table()
#    test_reflect()
#    test_tests()
#    test_types()
#end
#
##println(code_native(CRC.loop_word_ref, (Int, Uint32, Array{Uint8, 1}, Uint32,# Bool, Bool)))
#
##tests()
##time_table_size()
#time_libz()
