
 using CRC, ArgParse


function main(args)

    s = ArgParseSettings("Calculate the CRC for the given files")

    @add_arg_table s begin
        "--list", "-l"
        help = "list available CRC algorithms"
        action = :store_true
        "--decimal", "-d"
        help = "show checksums as decimal values (default is hex)"
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

    if parsed_args["list"]
        names = sort(collect(keys(ALL)), by=n->ALL[n])
        for name in names
            @printf("%s %s\n", name, ALL[name])
        end
    end

    name = symbol(parsed_args["crc"])
    if !haskey(ALL, name)
        error("CRC $name is not defined")
    end

    append = parsed_args["append"]
    spec = ALL[name]
    c = crc(spec)
    if parsed_args["decimal"]
        fmt = x -> dec(x)
    else
        fmt = x -> "0x" * hex(x, 1 + div(spec.width, 4))
    end
    sum = 0
    for file in parsed_args["files"]
        if file == "-"
            sum = c(STDIN, append=append)
        else
            sum = c(file, append=append)
        end
        if !append
            println("$(fmt(sum)) $file")
        end
    end
    if append
        println("$(fmt(sum))")
    end

end

main(ARGS)
