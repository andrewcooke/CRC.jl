
using CRC

width = 8
poly = 0x31
init = 0x00
refin = false
refout = false
xorout = 0x00
check = 0x00

s = spec(width, poly, init, refin, refout, xorout, check)
d = b"xyz"

for t in (NoTables, Single, Multiple)
    println(t)
    c = crc(s, tables=t)
    c(d)
    @time c(d)
end
