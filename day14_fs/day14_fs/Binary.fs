module day14_fs.Binary

open System
open day14_fs.Constants

type Bit = uint64
let ZERO = 0UL
let ONE = 1UL 

let toBit (c: char): Option<Bit> =
    match c with
    | '0' -> Some 0UL
    | '1' -> Some 1UL
    | _ -> None

let toBits (value: String): Option<Bit []> =
    let optionalBits =
        value.ToCharArray()
        |> Seq.map toBit
        |> Seq.toArray

    if optionalBits |> Seq.contains None then
        None
    else
        optionalBits
        |> Seq.map Option.get
        |> Seq.toArray
        |> Some

let fromBinary (value: String): Option<uint64> =
    printfn "fromBinary %s" value

    match toBits value with
    | None -> None
    | Some bits ->
        bits
        |> Seq.fold (fun num bit -> (num * 2UL) + bit) 0UL
        |> Some

let toBinary (value:uint64) : String =
    let toChar (i:Bit) : char = if i = ZERO then '0' else '1'
    let bitSeqRev = value |> Seq.unfold (fun (n:UInt64) -> if n = ZERO then None else Some(n % 2UL, n >>> 1))
    let bitSeq = bitSeqRev |> Seq.rev
    let bitChars = bitSeq |> Seq.map toChar 
    let bitString : String = bitChars |> String.Concat
    bitString 
    
let padTo (length: int) (bin:String) : String =
    if bin.Length >= length then
        bin
    else
        let missing = length - bin.Length
        let prefix = "0" |> String.replicate missing
        sprintf "%s%s" prefix bin
        
let toPaddedBinary (value:uint64):String =
    value |> toBinary |> padTo (NUM_LENGTH |> int)