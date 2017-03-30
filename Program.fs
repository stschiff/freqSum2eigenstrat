// Learn more about F# at http://fsharp.org

open System
open System.IO
open Argu

type CLArguments = 
    | [<AltCommandLine("-p")>] [<Unique>] EigenstratPosFile of path:string
    | [<Mandatory>] [<AltCommandLine("-o")>] OutPrefix of path:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | EigenstratPosFile _ -> "specify an eigenstrat position file to \
                constrain the output positions to those given in the file"
            | OutPrefix _ -> "specify the output prefix. Output files will be named \
                <prefix>.eigenstrat.ind.txt, <prefix>.eigenstrat.pos.txt and \
                <prefix>.eigenstrat.geno.txt"

type FreqSumRow = {chrom:string; pos:int; ref:char; alt:char; counts:int[]}

let parser = ArgumentParser.Create<CLArguments>(programName = "freqsum2eigenstrat")

let stdin =
    let sr = new StreamReader(Console.OpenStandardInput())
    seq {
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let readFreqSumHeader (s:seq<string>) =
    let header = Seq.head s
    let fields = header.Split [|' '; '\t'|]
    let namesAndNumbers =
        fields.[4..] |> Array.map (fun word ->
            match word.Split [|'('; ')'|] with
                | [|name; countS; _|] -> (name, int countS)
                | _ -> failwith ("could not parse " + word)
    )
    namesAndNumbers

let parseLine (line:string) =
    let fields = line.Split [|' '; '\t'|]
    let (chrom, pos, ref, alt) =
        match fields.[..3] with
        | [|chrom; posS; refS; altS|] -> (chrom, int posS, refS.[0], altS.[0])
        | _ -> failwith <| "could not parse line " + line
    let alleleCounts = fields.[4..] |> Array.map int
    {chrom = chrom; pos = pos; ref = ref; alt=alt; counts = alleleCounts}
    // let totalAlleleCount = Array.sum alleleCounts
    // if totalAlleleCount > 0 then printfn "%d %A" pos alleleCounts

writeIndFile (outf:string) header = 
    

let run (optPosFile:string option) (outPrefix:string) =
    printfn "running with parameters:"
    printfn "posFile: %A" optPosFile
    printfn "prefix: %s" outPrefix
    let outIndFile = outPrefix + ".eigenstrat.ind.txt"
    let outPosFile = outPrefix + ".eigenstrat.pos.txt"
    let outGenoFile = outPrefix + ".eigenstrat.geno.txt"
    
    let header = readFreqSumHeader stdin
    writeIndFile outIndFile header
    stdin
    |> Seq.map parseLine
    |> Seq.filter (fun fs -> Array.sum fs.counts > 0)
    |> 


[<EntryPoint>]
let main argv =
    try
        let parseResult = parser.Parse argv
        let prefix = parseResult.GetResult <@ OutPrefix @>
        let posFile =
            match parseResult.GetResults <@ EigenstratPosFile @> with
            | [] -> None
            | v::_ -> Some v
        run posFile prefix
        0
    with
    | :? ArguParseException as ex ->
        printfn "%O" ex.Message
        1
