module day14_fs.Binary

open System

type Bit = uint64

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
