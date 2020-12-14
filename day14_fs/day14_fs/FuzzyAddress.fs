module day14_fs.FuzzyAddress

    open System
    open Constants 
    type FuzzyAddress (addr: String) as self =
        override this.ToString () = sprintf "~%s" addr
        new(num:uint64) =
            let value = num |> Binary.toPaddedBinary
            FuzzyAddress (value)
        member this.Value = addr
        member this.chars () = addr.ToCharArray () 

    type FuzzyMask (mask: String) as self =
        override this.ToString () = sprintf "#%s" mask
        new () = FuzzyMask("0" |> String.replicate NUM_LENGTH)
        member this.applyTo(address:FuzzyAddress):FuzzyAddress =
            let maskBit (mask,bit) = if mask = '0' then bit else mask 
            let addrBits = address.Value
            let bits = Seq.zip mask addrBits
            let masked = bits |> Seq.map maskBit |> String.Concat
            FuzzyAddress masked
            
    let fuzzySize (addr:FuzzyAddress) =
        let str = addr.Value
        let xs = str.ToCharArray () |> Seq.filter (fun c -> c = 'X') |> Seq.length
        pown 2 xs 
        
    let intersects (first:FuzzyAddress) (second:FuzzyAddress) : bool =
        let intersectChars (c1:char) (c2:char) = c1 = c2 || c1 = 'X' || c2 = 'X'
        let chars1 = first.chars ()
        let chars2 = second.chars ()
        Seq.map2 intersectChars chars1 chars2 |> Seq.contains false |> not  
        