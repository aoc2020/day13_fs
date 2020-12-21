module day13.Types

type Bus (bus:int64,offset:int64) as self =
    override this.ToString() = sprintf "Bus(#%d +%d)" bus offset
    member this.No = bus
    member this.Offset = offset
    member this.compress () : Bus = Bus (bus, offset % bus)
