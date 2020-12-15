// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open day14_fs 
open day14_fs.Instructions
open day14_fs.FuzzyAddress

let task2 (program:Instruction[]) =
    let vm = VM2.execute program 
    // let answer = vm.memory |> Map.toSeq |> Seq.map snd |> Seq.sum
    printfn "RESULT: %A = %A" vm 42L  
  
let task1 (program:Instruction[]) =
    let vm = VM1.execute program 
    let answer = vm.memory |> Map.toSeq |> Seq.map snd |> Seq.sum
    printfn "RESULT: %A = %A" vm answer  

let addPath (fileName: String) : String =
    sprintf "/Users/xeno/projects/aoc2020/day14_fs/%s" fileName 

let unsafeLoad (fileName:String) : Instruction[] =
    let fullPath = addPath fileName
    let program = fullPath |> IO.readFile |> parseProgram
    Option.get program 

[<EntryPoint>]
let main argv =
    printfn "Hello world"
    let program = unsafeLoad "input.txt"
    let program2 = unsafeLoad "input2.txt"
    let program3 = unsafeLoad "input3.txt"

    task2 program3
    
//    let addr1 = FuzzyAddress("XXX1111")
//    let addr2 = FuzzyAddress("XXXX1X1")
//    let ints = splitIntersecting addr1 addr2
//    printfn "ints=%A" ints 
    0 // return an integer exit code
   