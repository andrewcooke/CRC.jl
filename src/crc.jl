
 using CRC, ArgParse


function main(args)

    s = ArgParseSettings("Calculate the CRC for the given files")

    @add_arg_table s begin
        "--list", "-l"
        help = "list available CRC algorithms"
        action = :store_true
        "--crc", "-c"
        help = "name the CRC to use"
        default = "CRC_32"
        "--append", "-a"
        help = "append the data together"
        action = :store_true
        "files"
        help = "the files to read (- for stdin)"
        nargs = '*'
    end

    parsed_args = parse_args(args, s)
    println("Parsed args:")
    for (key, val) in parsed_args
        println(" $key => $(repr(val))")
    end

    if parsed_args["list"]
        names = sort(collect(keys(ALL)), by=n->ALL[n])
        for name in names
            @printf("%20s: %s\n", name, ALL[name])
        end
    end

    name = symbol(parsed_args["crc"])
    if !haskey(ALL, name)
        error("CRC $name is not defined")
    end

    c = crc(ALL[name])
    for file in parsed_args["files"]
        sum = c(file, append=parsed_args["append"])
        println("$sum $file")
    end

end

main(ARGS)
