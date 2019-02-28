export main

using ArgParse
using Printf

# julia -e "Pkg" ...

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
        help = "combine the data from all files"
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

    name = Symbol(parsed_args["crc"])
    if !haskey(ALL, name)
        error("CRC $name is not defined")
    end

    append = parsed_args["append"]
    spec = ALL[name]
    c = crc(spec)
    if parsed_args["decimal"]
        fmt = x -> Int(x)
    else
        fmt = x -> "0x" * string(x, base=16, pad = 1 + div(spec.width-1, 4))
    end
    sum = 0
    for file in parsed_args["files"]
        if file == "-"
            sum = c(STDIN, append=append)
        else
            open(file, "r") do f
                sum = c(f, append=append)
            end
        end
        if !append
            println("$(fmt(sum)) $file")
        end
    end
    if append
        println("$(fmt(sum))")
    end

end
