module day14_fs.FuzzyAddress

    open System
    
    type FuzzyMask (mask: String) as self =
        override this.ToString () = sprintf "#%s" mask 
    
    type FuzzyAddress (addr: String) as self =
        override this.ToString () = sprintf "~%s" addr
        
    let mask (address:int64) (mask:FuzzyMask) : FuzzyAddress = FuzzyAddress("NIL")
        
        
        

