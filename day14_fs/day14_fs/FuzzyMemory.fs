module day14_fs.FuzzyMemory

    open day14_fs.FuzzyAddress
    open day14_fs.Instructions

    let intersectsOp (op1:Op) (op2:Op) : bool =
        match (op1,op2) with
        | FuzzyStore(addr1,_),FuzzyStore(addr2,_) ->
            FuzzyAddress.intersects addr1 addr2
        | _ -> false 
        
    type MemArea (address:FuzzyAddress, distinct:bool, value:uint64) as self =
        override this.ToString () =
            let pureChar = if distinct then "•" else "#"
            sprintf "[%A:A%s=%d]" address pureChar value 
        member this.Distinct = distinct
        member this.Value = value
        member this.Address = address
        member this.asPure () = MemArea(address, true, value)
        member this.asDirty () = MemArea(address, false, value)
        member this.withPurity (purity:bool) = MemArea(address, purity, value)
        member this.intersectsWith (addr:FuzzyAddress) = FuzzyAddress.intersects addr address
        member this.intersectsWith (mem:MemArea) = FuzzyAddress.intersects mem.Address address
        member this.modifyDistinct (newDist:bool) = MemArea(address,distinct && newDist,value)
        member this.isShadowedBy (mem:MemArea) = fullyContains mem.Address address
        member this.shadows (mem:MemArea) = fullyContains address mem.Address
    
    let splitByMultipleMemoryAreas (mem:MemArea[]) (addr:FuzzyAddress) : FuzzyAddress[] =  
        let rec split (mems:List<MemArea>) (addr:FuzzyAddress): seq<FuzzyAddress> =
            match mems with
                | [] -> [|addr|] |> Array.toSeq 
                | m :: moreMems ->
                    let splitUp : FuzzyAddress[] = splitIntersecting m.Address addr
                    let furtherSplitUp : seq<seq<FuzzyAddress>> = splitUp |> Seq.map (split moreMems)
                    furtherSplitUp |> Seq.concat
        if mem.Length = 0 then
            [|addr|]
        else 
            let memList = mem |> List.ofSeq
            split memList addr |> Seq.toArray
            
    let splitShallowByMultipleAddresses (mem:MemArea[]) (addr:FuzzyAddress) : FuzzyAddress[] =
        let rec split (mems:List<MemArea>) (addr:FuzzyAddress): FuzzyAddress[] =
            match mems with
                | [] -> [|addr|] 
                | m :: moreMems ->
                    let splitUp : FuzzyAddress[] = splitIntersecting m.Address addr                    
                    if splitUp.Length > 1 then
                        splitUp
                    else
                        split moreMems addr
        let memList = mem |> List.ofSeq
        split memList addr |> Seq.toArray        
    
    type FMemory (mem: MemArea[]) as self =
        override this.ToString () =
            let line1 = sprintf "  FuzzyMemory:\n"
            let lines = mem |> Seq.map (sprintf "    %A") |> String.concat "\n"
            sprintf "%s%s" line1 lines 
        new () = FMemory ([||])
        
        member this.Mem = mem 
       
        member this.addUnchecked (memArea:MemArea) : FMemory =
            let newMem = Seq.append mem [memArea] |> Seq.toArray 
            FMemory (newMem)
            
        member this.addUnchecked (memAreas:MemArea[]) : FMemory =
            let newMem = Seq.append mem memAreas |> Seq.toArray
            FMemory (newMem)

        member this.splitMemoryBy (addr:FuzzyAddress) : FMemory =
            let split (area:MemArea) =
                let addresses = splitIntersecting addr area.Address
                addresses |> Seq.map (fun addr -> MemArea(addr,area.Distinct,area.Value))
            mem |> Seq.map split |> Seq.concat |> Seq.toArray |> (FMemory)
            
        member this.splitByMemory (addr:FuzzyAddress) : FuzzyAddress[] =
            splitShallowByMultipleAddresses mem addr
            
        member this.add(op:Op) : FMemory =
            match op with
            | Noop -> self
            | FuzzyStore(addr,value) ->
                let memArea = MemArea(addr,true,value)
                this.addUnchecked memArea

        member this.removeShadowed () : FMemory =
            let stack = mem |> Seq.toList
            let rec filterShadowed (memList:List<MemArea>) : List<MemArea> = 
                match memList with
                | a :: tail ->
                    let isShadowed = tail |> Seq.filter (a.isShadowedBy) |> Seq.isEmpty |> not
                    let filteredTail = filterShadowed tail
                    if isShadowed then
                        filteredTail
                    else
                        a :: filteredTail
                | [] -> []
            let newMem = filterShadowed stack |> Seq.toArray
            FMemory(newMem)
            
        member this.checkConflicts () : FMemory =            
            let conflictsWith (a1:int*MemArea) (a2:int*MemArea):bool =
                if (fst a1) = (fst a2)
                then false // does not conflict with itself
                else
                    let m1 = (snd a1)
                    let m2 = (snd a2)
                    m1.intersectsWith m2
            let isDistinct (others:(int*MemArea)[]) (a1:int*MemArea) : bool =
                others |> Seq.filter (conflictsWith a1) |> Seq.isEmpty             
            let pairToMarked (a1:int*MemArea) (distinct:bool) =
                (snd a1).withPurity distinct 
            let indexed = mem |> Seq.mapi (fun i area -> (i,area)) |> Seq.toArray 
            let newMem = indexed |> Seq.map (fun a1 -> pairToMarked a1 (isDistinct indexed a1)) |> Seq.toArray
            FMemory(newMem) 
              