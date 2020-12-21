module day13.Task2

open day13.Types
open day13.MyMath 

type TimeBox (k:int64, var:int64) as self =
    override this.ToString() = sprintf "TimeBox(%d,x*%d)" k var
    member this.K = k
    member this.Var = var
    new (b:Bus) =
        let offset = if b.Offset = 0L then b.No else b.Offset 
        TimeBox(offset, b.No)
    member this.adjustFor (box:TimeBox) =
        let newConst = scpWithK var (k+box.K) box.Var 0L
        let newConst = newConst - box.K 
        let additionals = scp var box.Var  
        TimeBox (newConst, additionals) 

let solve (busses:Bus[]) : int64 =
    let mergeBoxes (box1:TimeBox) (box2:TimeBox) =
//        printfn "Merge: %A with %A" box1 box2 
        box1.adjustFor box2
    let boxes = busses |> Seq.map (TimeBox)
//    printfn "boxes: %A" boxes 
    let result = boxes |> Seq.reduce mergeBoxes
//    printfn "Result: %A" result
    result.K 
    
    
        

