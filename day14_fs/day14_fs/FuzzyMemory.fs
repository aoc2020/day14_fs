module day14_fs.FuzzyMemory

    open day14_fs.FuzzyAddress

    type FMemory (content: FuzzyAddress[]) as self =
        override this.ToString () = "FMemory"
        new () = FMemory ([||])
        