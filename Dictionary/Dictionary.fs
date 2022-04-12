module Dictionary

type Dictionary =
     | D of Map<char,Dictionary> * bool

let empty () = D(Map.empty, false)

let rec insert (s:string) (D(m,b)) =
    match s with
    | "" -> D(m,true)
    | _  ->
        let c = s.[0]
        let d = Map.tryFind c m
        match d with
        | None   -> D(Map.add c (empty () |> insert s.[1..]) m,b)
        | Some d -> D(Map.add c (d        |> insert s.[1..]) m,b)

let rec lookup s (D(m,b)) =
    match s with
    | "" -> b
    | _  ->
        let d = Map.tryFind s.[0] m
        match d with
        | None   -> false
        | Some d -> lookup s.[1..] d

let step c (D(m,_)) =
    let dict' = Map.tryFind c m
    match dict' with
    | None                -> None
    | Some (D(_,b') as d) -> Some (b', d)