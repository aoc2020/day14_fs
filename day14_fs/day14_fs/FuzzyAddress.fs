module day14_fs.FuzzyAddress

    open System
    open Constants 
    
    type FuzzyAddress (addr: String) as self =
        override this.ToString () = sprintf "~%s" addr
        new(num:uint64) =
            let value = num |> Binary.toPaddedBinary
            FuzzyAddress (value)
        member this.getValue () = addr 

    type FuzzyMask (mask: String) as self =
        override this.ToString () = sprintf "#%s" mask
        new () = FuzzyMask("0" |> String.replicate NUM_LENGTH)
        member this.applyTo(address:FuzzyAddress):FuzzyAddress =
            let maskBit (mask,bit) = if mask = '0' then bit else mask 
            let addrBits = address.getValue ()
            let bits = Seq.zip mask addrBits
            let masked = bits |> Seq.map maskBit |> String.Concat
            FuzzyAddress masked         
        
        

