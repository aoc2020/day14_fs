module day14_fs.Instructions

open System

type Mem (address:int64,value:int64) as self =
    member this.getAddress = address
    member this.getValue = value
    
type Mask (value: String) as self =
    member this.getValue = value
    
type Instruction =
    | Mem of int64*int64
    | Mask of String 

let parseInstruction (s:String) : Option<Instruction> =
    match s with
    | _ when s.Contains("mask =") -> 
        let split = s.Split '=' 
        in Mask (split.[1].Trim()) |> Some 
    | _ when s.Contains("mem[") -> 
        let split = s.Split '='
        let split2 = split.[0].Split ']'
        let split3 = split2.[0].Split '['
        let inst = split3.[1]
        let address = split3.[1].Trim () |> int64
        let value = split.[1].Trim() |> int64
        in Mem(address,value) |> Some
    | _ ->
        printfn "Invalid instruction: %s" s 
        None
        
let parseProgram (input:seq<string>): Option<Instruction[]> =
    let inst = input |> Seq.map parseInstruction |> Seq.toArray 
    if inst |> Seq.contains None then
        None
    else
        inst |> Seq.map Option.get |> Seq.toArray |> Some  