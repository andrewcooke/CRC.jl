
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
    @test crc(spec, lookup=false)(TEST) == spec.test
    print(".")
    result = crc(spec)(TEST)
    if result != spec.test
        println("false $(hex(result))")
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

function test_all()
    print("all")
    bad = Set()
    for _ in 1:10
        data = rand(Uint8, rand(100:200))
#        data = rand(Uint8, rand(10:20))
        for spec in ALL
            if ! in(spec, bad)
#                r1 = crc(spec, data)
                r1 = crc(spec, lookup=false)(data)
                r2 = crc(spec)(data)
                if r1 != r2
                    push!(bad, spec)
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

    ours = crc(CRC_32)
    data = rand(Uint8, 300_000_000)
    check = crc32(data)
    @assert ours(data) == check

    @time crc32(data)   # 0.28
    @time ours(data)    # 0.67
end

function time_padded()
    ours = crc(CRC_64)
    data = rand(Uint8, 300_000_000)
    @assert ours(TEST) == CRC_64.test
    @time ours(data)    # 
end


srand(0)  # repeatable results

#time_libz()
time_padded()
