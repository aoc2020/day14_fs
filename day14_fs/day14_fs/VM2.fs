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
    member this.optimizeAtBit (bit:int) =
        let size1 = memory.Mem.Length
        let step1 = memory.removeShadowed ()
        let size2 = step1.Mem.Length 
        let step2 = step1.checkConflicts ()
        let size3 = step2.Mem.Length 
        let step3 = step2.moveDistinct ()
        let size4 = step3.Mem.Length 
        let step4 = step3.expandAtBit bit
        printfn "OPTIMIZE(%d): SHADOW: rm %d -> %d dist: mv %d -> %d" bit (size1-size2) size2 (size3-size4) size4 
        VM (step4)       
    member this.optimize () =
        let optimize (vm:VM) (bit:int) = vm.optimizeAtBit bit 
        let optimizedVM = [0..36] |> Seq.fold optimize self
        let distinctMem = optimizedVM.Memory.DistinctMem
        printfn "Split into %d distinct memory segments" distinctMem.Length 
        optimizedVM

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

let sumMemory (vm:VM) : uint64 =
    let mem = vm.Memory.DistinctMem
    let sumOfArea (m:MemArea) =
        let size = m.Address.spanSize ()
        let value = m.Value
        let areaTotal = m.Address.spanSize () * m.Value 
        printfn "$ %A ^%d = %d -> %d" m.Address size value areaTotal
        areaTotal
    let sum = mem |> Seq.map sumOfArea |> Seq.sum
    printfn "SUM: %d" sum 
    sum 