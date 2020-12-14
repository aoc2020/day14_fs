// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let readFile (filePath:String) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let fromBinary (value:String):int64 =
    printfn "fromBinary %s" value 
    let charToInt (c:char) : int64 =
        match c with
        |'0' -> 0L
        |'1' -> 1L
        | _ -> -9999L
    let bits = value.ToCharArray() |> Seq.map charToInt |> Seq.toArray 
    let z = bits |> Seq.fold (fun acc v -> (acc * 2L) + v) 0L
//    printfn "fromBinary1: %A" bits
//    printfn "fromBinary2: %A" z
    z

type Instruction(name:String,index:Option<int64>,value:String) as self =
    member this.name = name
    member this.index = index
    member this.value = value
    new (s:String) =
        match s with
        | _ when s.Contains("mask =") -> 
            let split = s.Split '=' 
            in Instruction(split.[0].Trim(),None,split.[1].Trim())
        | _ when s.Contains("mem[") -> 
            let split = s.Split '='
            let split2 = split.[0].Split ']'
            let split3 = split2.[0].Split '['
            let inst = split3.[1]
            let index = split3.[1].Trim () |> int64
            in Instruction(split3.[0].Trim(),Some index,split.[1].Trim())
        | _ -> Instruction("Unknown",None,"Invalid")
    override this.ToString () : String =
        sprintf "Instruction(%A,%A,%A)" name index value

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
        match inst.name with 
        | "mask" -> VM (memory, Mask inst.value)
        | "mem" ->
            let value = inst.value |> int64
            let zeroes = mask.getZeroes            
            let ones = mask.getOnes
//            printfn "ONES: %A" ones 
//            printfn "ZEROES: %A" zeroes
            let address =
                match inst.index with
                |Some(a) -> a
                |None -> -1L
            let newValue = (value ||| ones) &&& zeroes
            printfn "VAL %d -> %d" value newValue
            let newMem = memory.Add(address, newValue)
            VM (newMem,mask)
    override this.ToString () = sprintf "VM(%A)" memory 

// Define a function to construct a message to print

let task1 (program:Instruction[]) =
    let vm = VM ()
    let result = program |> Seq.fold (fun (vm:VM) -> vm.exec) vm
    let answer = result.memory |> Map.toSeq |> Seq.map snd |> Seq.sum
    printfn "RESULT: %A = %A" result answer  

[<EntryPoint>]
let main argv =
    printfn "Hello world"
    let input = readFile "/Users/xeno/projects/aoc2020/day14_fs/input.txt" |> Seq.toArray
    printfn "%A" input 
    let program = input |> Seq.map Instruction |> Seq.toArray 
    printfn "%A" program
    let test = fromBinary "000000101"
    printfn "%A" test
    task1 program 
    0 // return an integer exit code