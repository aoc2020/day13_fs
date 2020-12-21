module day13.Task1

open System

let private earliest (bustime1:int64*int64) (bustime2:int64*int64) : int64*int64 =
    let (_,time1) = bustime1
    let (_,time2) = bustime2
    if time2 < time1 then bustime2 else bustime1 

let task1 (input:String[]):int64 = 
    let time = input.[0] |> int64
    let busses = input.[1].Split ',' |> Seq.filter (fun bus -> not (bus = "x")) |> Seq.map int64 |> Seq.toArray
    let leftovers = busses |> Seq.map (fun bus -> bus - (time % bus) + time) |> Seq.toArray     
    let bustimes = leftovers |> Seq.mapi (fun i time -> (busses.[i],time)) |> Seq.toArray
    let (bus,bustime) = bustimes |> Seq.reduce earliest
    let answer = bus * (bustime-time)
    answer 

