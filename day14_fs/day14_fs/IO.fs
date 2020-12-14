module day14_fs.IO

open System
open System.IO

let readFile (filePath:String) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}


