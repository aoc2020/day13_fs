module day13.MyMath

let rec gcd (a:int64) (b:int64) : int64 =
    if a = b then a
    else if a > b
         then gcd (a-b) b
         else gcd (b-a) a  

let rec private scpRec (a:int64) (ap:int64) (b:int64) (bp:int64) : int64 =
    if ap = bp then ap 
    else if ap > bp then
            let delta = ap - bp           
            let new_bp = bp + ((((delta-1L) / b)+1L) * b)
            scpRec a ap b new_bp               
         else
            scpRec b bp a ap 

let scp (a:int64) (b:int64) : int64 =
    scpRec a a b b 
    
let scpWithK (a:int64) (ak:int64) (b:int64) (bk:int64) : int64 =
    let ak = if ak = 0L then a else ak
    let bk = if bk = 0L then b else bk 
    scpRec a ak b bk
     