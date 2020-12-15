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
    member this.optimize () =
        let step1 = memory.removeShadowed ()
        let step2 = step1.checkConflicts ()
        let step3 = step2.moveDistinct ()
        let step4 = step3.expandAtBit 0
        VM (step3)       

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
    let naiveVM = program |> Seq.fold (fun (vm:VM) (op:Op) -> vm.exec op) emptyVM
    let vm = naiveVM
    printfn "Compiled: %A" program
    printfn "VM = %A" naiveVM
    let optimized = vm.optimize ()
    optimized 
