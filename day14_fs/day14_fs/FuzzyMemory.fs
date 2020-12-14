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
        
        member this.add (op:Op) : FMemory =
            match op with
            | Noop -> self
            | FuzzyStore(addr,value) ->
                let isSafe (area:MemArea) = area.intersectsWith addr
                let applySafety (dist:bool) (area:MemArea) : MemArea*bool =
                    let safe = isSafe area
                    let newArea = area.modifyDistinct safe
                    let distinct = dist && safe
                    (newArea,distinct)
                let result : seq<MemArea>*bool = mem |> Seq.mapFold applySafety true
                let thisArea = MemArea (addr, snd result,value)
                let newMem = Seq.append (fst result) [thisArea] |> Seq.toArray
                FMemory(newMem)