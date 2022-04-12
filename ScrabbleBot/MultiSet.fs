module internal MultiSet
type MultiSet<'a> when 'a : comparison =
    | MS of Map<'a, uint32> * uint32

(* Green *)
let empty = MS (Map.empty, 0u)

let isEmpty (MS(m', _)) = Map.isEmpty m'

let size (MS(_, n')) = n'

let contains a (MS(m', _)) = Map.containsKey a m'

let numItems a (MS(m', _)) =
    Map.tryFind a m' |> function
                        | None   -> 0u
                        | Some n -> n

let add a n (MS(m', n') as s) =
    MS (Map.add a ((numItems a s) + n) m', n' + n)

let addSingle a s = add a 1u s

let remove a n (MS(m', n') as s) =
    let currentNr = numItems a s
    match n with
    | n when n < currentNr -> MS(Map.add a ((numItems a s) - n) m', n' - n)
    | _ -> MS(Map.remove a m', n' - currentNr)

let removeSingle a s = remove a 1u s

let fold f acc (MS(m', _)) = Map.fold f acc m'

let foldBack f (MS(m', _)) acc = Map.foldBack f m' acc

(* Yellow *)
// nessesary because CodeJudge doesn't understand Map.keys
let toSet (MS(m1',_)) = Map.toSeq m1' |> Seq.map fst |> set

let ofList lst = List.fold (fun s a -> addSingle a s) empty lst

let toList s =
    let rec aux = function
                  | (lst, a, n) when n > 0u -> a::aux(lst, a, n-1u)
                  | (lst, _, _) -> lst
    foldBack (fun a n lst -> aux(lst, a, n)) s List.empty

let map f s = fold (fun s a n -> add (f a) n s) empty s

let union s1 s2 =
    let keys = Set.union (toSet s1) (toSet s2)
    Set.fold (fun s a -> add a (max (numItems a s1) (numItems a s2)) s) empty keys

let sum s1 s2 = fold (fun s a n -> add a n s) s1 s2

let subtract s1 s2 = fold (fun s a n -> remove a n s) s1 s2

let intersection s1 s2 =
    let keys = Set.intersect (toSet s1) (toSet s2)
    Set.fold (fun s a -> add a (min (numItems a s1) (numItems a s2)) s) empty keys