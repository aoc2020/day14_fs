module day14_fs.FuzzyMemory

    open day14_fs.FuzzyAddress
    open day14_fs.Instructions

    let intersectsOp (op1:Op) (op2:Op) : bool =
        match (op1,op2) with
        | FuzzyStore(addr1,_),FuzzyStore(addr2,_) ->
            FuzzyAddress.intersects addr1 addr2
        | _ -> false 
    
    
    type MemArea (address:FuzzyAddress, distinct:bool, value:uint64) as self =
        override this.ToString () = sprintf "Memory(%A,pure=%A,value=%A" address distinct value 
        member this.Distinct = distinct
        member this.Value = value
        member this.Address = address
        member this.intersectsWith (addr:FuzzyAddress) = FuzzyAddress.intersects addr address
        member this.intersectsWith (mem:MemArea) = FuzzyAddress.intersects mem.Address address
        member this.modifyDistinct (newDist:bool) = MemArea(address,distinct && newDist,value)       
        
    
    type FMemory (mem: MemArea[]) as self =
        override this.ToString () = sprintf "FMemory(%A)" mem
        new () = FMemory ([||])
        
        member this.Mem = mem 
        
//        let splitOnIntersects (addr:FuzzyAddress) = 0
        member this.removeShadowedBy (addr:FuzzyAddress) : FMemory =
            let stillVisible (area:MemArea) = fullyContains addr (area.Address) |> not 
            let newMem = mem |> Seq.filter stillVisible |> Seq.toArray  
            FMemory (newMem) 

        member this.checkAndUpdateSafety (addr:FuzzyAddress) : FMemory * bool =
            let checkIfSafe (area:MemArea) = area.intersectsWith addr
            let applySafety (dist:bool) (area:MemArea) : MemArea*bool =
                let safe = checkIfSafe area
                let newArea = area.modifyDistinct safe
                let distinct = dist && safe
                (newArea,distinct)
            let result : seq<MemArea>*bool = mem |> Seq.mapFold applySafety true
            let isSafe = snd result
            let newMem = fst result |> Seq.toArray
            (FMemory(newMem),isSafe)

        member this.addUnchecked (memArea:MemArea) : FMemory =
            let newMem = Seq.append mem [memArea] |> Seq.toArray 
            FMemory (newMem)
            
        member this.add (op:Op) : FMemory =
            match op with
            | Noop -> self
            | FuzzyStore(addr,value) ->
                let memStep1 = this.removeShadowedBy addr
                let memAndSafety = memStep1.checkAndUpdateSafety addr
                let memStep2 = fst memAndSafety
                let isSafe = snd memAndSafety 
                let thisArea = MemArea (addr, isSafe, value)
                let memStep3 = memStep2.addUnchecked thisArea 
                memStep3 
