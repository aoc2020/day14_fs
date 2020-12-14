module day14_fs.VM1

open System
open Binary 
open day14_fs.Instructions


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
    member this.getOnes = ones |> fromBinary |> Option.get
    member this.getZeroes = zeroes |> fromBinary |> Option.get
    
type VM (memory:Map<uint64,uint64>, mask: Mask) as self =
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
