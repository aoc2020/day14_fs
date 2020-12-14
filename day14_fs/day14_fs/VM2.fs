module day14_fs.VM2

open System
open day14_fs.FuzzyAddress
open day14_fs.FuzzyMemory
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
  

    
type VM (memory:FMemory) as self =
    override this.ToString () = sprintf "VM(%A)" memory
    new() = VM (FMemory ())
    member this.Memory = memory 
    member this.exec (op:Op) : VM =
        match op with
        | Noop -> self
        | FuzzyStore _ -> VM(memory.add op) 
        

let compile (program:Instruction[]):Op[] =
    let compileInstruction (mask:FuzzyMask) (instruction:Instruction) : Op*FuzzyMask = 
        match instruction with
            | Mask newMask -> (Noop,FuzzyMask newMask)
            | Mem (address,value) ->
                let maskedAddress = FuzzyAddress address |> mask.applyTo
                let fuzzyStore = FuzzyStore (maskedAddress,value)
                (fuzzyStore,mask)
    program |> Seq.mapFold compileInstruction (FuzzyMask ()) |> fst |> Seq.toArray   

let execute (source: Instruction[]) : VM =
    let emptyVM = VM ()
    let program = compile source
    let vm = program |> Seq.fold (fun (vm:VM) (op:Op) -> vm.exec op) emptyVM
    printfn "Compiled: %A" program
    printfn "VM = %A" vm 
//    program |> Seq.fold (fun (vm:VM) -> vm.exec) vm
    vm 
