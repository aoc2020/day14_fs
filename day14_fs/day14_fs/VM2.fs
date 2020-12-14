module day14_fs.VM2

open System
open day14_fs.Instructions

let fromBinary (value:String):int64 =
    printfn "fromBinary %s" value 
    let charToInt (c:char) : int64 =
        match c with
        |'0' -> 0L
        |'1' -> 1L
        | _ -> -9999L
    let bits = value.ToCharArray() |> Seq.map charToInt |> Seq.toArray 
    let z = bits |> Seq.fold (fun acc v -> (acc * 2L) + v) 0L
    z

type Mask (value:String) as self =
    let xTo (target:char) (c:char)=
        match c with
        | 'X' -> target
        | v -> v
    let xToZero = xTo '0'
    let xToOne = xTo '1'
    let ones : String = value.ToCharArray () |> Seq.map xToZero |> Seq.toArray |> String 
    let zeroes : String = value.ToCharArray () |> Seq.map xToOne |> Seq.toArray |> String           
    member this.toString = sprintf "#%s" value
    member this.getOnes = ones |> fromBinary
    member this.getZeroes = zeroes |> fromBinary
    
type VM (memory:Map<int64,int64>, mask: Mask) as self =
    new() = VM (Map.empty, Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    member this.memory = memory 
    member this.exec (inst:Instruction) : VM =
        match inst with 
        | Mask mask -> VM (memory, Mask mask)
        | Mem (address,value) ->
            let value = value
            let zeroes = mask.getZeroes            
            let ones = mask.getOnes
//            printfn "ONES: %A" ones 
//            printfn "ZEROES: %A" zeroes
            let newValue = (value ||| ones) &&& zeroes
            printfn "VAL %d -> %d" value newValue
            let newMem = memory.Add(address, newValue)
            VM (newMem,mask)
    override this.ToString () = sprintf "VM(%A)" memory

let execute (program: Instruction[]) : VM =
    let vm = VM ()
    program |> Seq.fold (fun (vm:VM) -> vm.exec) vm
