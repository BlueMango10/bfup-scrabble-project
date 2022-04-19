module internal MultiSet
    
    type MultiSet<'a> when 'a : comparison
    
    val empty        : MultiSet<'a>
    val isEmpty      : s: MultiSet<'a> -> bool
    val size         : s: MultiSet<'a> -> uint32
    val contains     : a: 'a -> s: MultiSet<'a> -> bool
    val numItems     : a: 'a -> s: MultiSet<'a> -> uint32
    val add          : a: 'a -> n: uint32 -> s: MultiSet<'a> -> MultiSet<'a>
    val addSingle    : a: 'a -> s: MultiSet<'a> -> MultiSet<'a>
    val remove       : a: 'a -> n: uint32 -> s: MultiSet<'a> -> MultiSet<'a>
    val removeSingle : a: 'a -> s: MultiSet<'a> -> MultiSet<'a>
    val fold         : f: ('a -> 'b -> uint32 -> 'a) -> acc: 'a -> s: MultiSet<'b> -> 'a
    val foldBack     : f: ('a -> uint32 -> 'b -> 'b) -> s: MultiSet<'a> -> acc: 'b -> 'b
    
    val ofList       : lst: 'a list -> MultiSet<'a>
    val toList       : s: MultiSet<'a> -> 'a list
    val map          : f: ('a -> 'b) -> s: MultiSet<'a> -> MultiSet<'b>
    val union        : s1: MultiSet<'a> -> s2: MultiSet<'a> -> MultiSet<'a>
    val sum          : s1: MultiSet<'a> -> s2: MultiSet<'a> -> MultiSet<'a>
    val subtract     : s1 :MultiSet<'a> -> s2: MultiSet<'a> -> MultiSet<'a>
    val intersection : s1: MultiSet<'a> -> s2: MultiSet<'a> -> MultiSet<'a>