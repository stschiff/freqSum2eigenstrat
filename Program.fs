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
            | EigenstratPosFile _ -> "specify a position file to \
                constrain the output positions to those given in the file. Must have two \
                columns: chromosome and position"
            | OutPrefix _ -> "specify the output prefix. Output files will be named \
                <prefix>.eigenstrat.ind.txt, <prefix>.eigenstrat.pos.txt and \
                <prefix>.eigenstrat.geno.txt"

type FreqSumRow = {chrom:int; pos:int; ref:char; alt:char; counts:int[]}

let parser = ArgumentParser.Create<CLArguments>(programName = "freqsum2eigenstrat")

let stdin =
    let sr = new StreamReader(Console.OpenStandardInput())
    seq {
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let readFreqSumHeader (s:string seq) =
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
        | [|chrom; posS; refS; altS|] -> (int chrom, int posS, refS.[0], altS.[0])
        | _ -> failwith <| "could not parse line " + line
    let alleleCounts = fields.[4..] |> Array.map int
    {chrom = chrom; pos = pos; ref = ref; alt=alt; counts = alleleCounts}

let writeIndFile outf header = 
    use sw = new StreamWriter(File.OpenWrite(outf))
    header |> Array.iter (fun (name, count) ->
        if count <> 2 then failwith "Cannot handle groups. Every individual needs to be separate."
        sw.WriteLine(name + "\t" + "U\tUnknown")
    )


let comparePos (chrom, pos) fs =
    match chrom - fs.chrom with
        | 0 -> pos - fs.pos
        | x -> x 

let filterFreqSumPos posSeq fsSeq = seq {
    for pair in OrderedZip.orderedZip posSeq fsSeq comparePos do
        match pair with
        | (Some pos, Some fs) -> yield fs
        | _ -> ()
}

let createOutPosSeq f = seq {
    use sr = new StreamReader(File.OpenRead(f))
    while not sr.EndOfStream do
        let line = sr.ReadLine()
        let fields = line.Split [|' '; '\t'|] |> Array.map int
        yield (fields.[0], fields.[1])
}

let writeEigenstratOut fsSeq outPos outGeno =
    use genoSW = new StreamWriter(File.OpenWrite(outGeno))
    use posSW = new StreamWriter(File.OpenWrite(outPos))
    fsSeq |> Seq.iter (fun fs ->
        let posLine =
            sprintf "%d_%d\t%d\t0.0\t%d\t%c\t%c" fs.chrom fs.pos fs.chrom fs.pos fs.ref fs.alt
        let genoLine =
            fs.counts
            |> Array.map (function
                | 0 -> '2'
                | 1 -> '1'
                | 2 -> '0'
                | _ -> '9')
            |> System.String
        genoSW.WriteLine(genoLine)
        posSW.WriteLine(posLine))

let run (optEigenstratPosFile:string option) (outPrefix:string) =
    let outIndFile = outPrefix + ".eigenstrat.ind.txt"
    let outPosFile = outPrefix + ".eigenstrat.snp.txt"
    let outGenoFile = outPrefix + ".eigenstrat.geno.txt"
    eprintfn "%s" <| "Writing genotype data to " + outGenoFile
    eprintfn "%s" <| "Writing position data to " + outPosFile
    eprintfn "%s" <| "Writing individual data to " + outIndFile
    
    let header = readFreqSumHeader stdin
    writeIndFile outIndFile header
    let freqSumSeq =
        stdin
        |> Seq.map parseLine
        |> Seq.filter (fun fs -> fs.counts |> Array.filter (fun v -> v >= 0) |> Array.sum > 0)
    let finalFreqSumSeq =
        match optEigenstratPosFile with
            | Some f ->
                let outPosSeq = createOutPosSeq f
                filterFreqSumPos outPosSeq freqSumSeq
            | None -> freqSumSeq
    writeEigenstratOut finalFreqSumSeq outPosFile outGenoFile

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
        eprintfn "%O" ex.Message
        1
