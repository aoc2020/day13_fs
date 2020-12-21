open System
open System.IO

open day13.Task2
open day13.Types 
open day13.IO

let enum_busses (input:String) : (int64*int64)[] =
    let toPair a b = (a,b)
    let enumerated = input.Split(',') |> Seq.mapi toPair |> Seq.toArray
    enumerated |> Seq.filter (fun i ->
        let (_, bus) = i
        not (bus = "x") 
        ) |> Seq.map (fun i ->
            let (index,bus) = i
            (index |> int64, bus |> int64)
        ) |> Seq.toArray 

let test (ibus:int64*int64) (time:int64): bool =
    let (i,bus) = ibus
    let rem = time % bus
    if rem = 0L && i > 0L then
        false
    else 
    let past_time =  bus - rem
    let timex = if past_time = bus then 0L else past_time 
    timex = i 

let rec scan (busses:(int64*int64)[]) (curr:int64) (fact:int64) : int64 =
        let invalidTime (ibus:(int64*int64)) : bool = test ibus curr |> not  
        let ok  = (busses |> Seq.tryFind invalidTime).IsNone                              
        if ok then
            curr
        else
            scan busses (curr+fact) fact

let busSortKey (ibus:int64*int64):int64 =
    let (i,bus) = ibus
    bus 
let sortBusses (busses:(int64*int64)[]): (int64*int64)[] =
    busses |> Seq.sortBy busSortKey |> Seq.rev |>  Seq.toArray 

let task2 (input:String[]) : int64 =
    let busses = enum_busses input.[1]
//    let busses = sortBusses busses <- must not be run, ruins everything
    let busses : Bus[] = busses |> Array.map (fun (enum:int64*int64) -> Bus(snd enum,fst enum).compress ())
//    printfn "%A" busses
    solve busses   

[<EntryPoint>]
let main argv =
    let input = read_file "/Users/xeno/projects/aoc2020/day13_fs/input.txt" |> Seq.toArray
//    let answer1 = task1 input
    let answer2 = task2 input
    printfn "Answer: %d" answer2
//    let box1 = TimeBox(Bus(17L,0L))
//    let box2 = TimeBox(Bus(13L,2L))
//    let box12 = box1.adjustFor box2
//    let box3 = TimeBox(Bus(19L,3L))
//    let box123 = box12.adjustFor box3 
//    printfn "merged: %A+%A=%A +%A=%A" box1 box2 box12 box3 box123 
       
    0