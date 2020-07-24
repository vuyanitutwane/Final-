#if !INTERACTIVE
module Patterns

open System

#endif
type Cell = 
| Black
| White
| Unknown

let toCells v =

    let k = v.ToString()
    let lst = Array.toList(k.ToCharArray())

    let cln a =
        match a.ToString() with 
        | "b" | "B" -> Black
        | "w" | "W" -> White
        | _ -> Unknown

    List.map cln lst
   
let fromCells v = 
    let rec build lst str =
        match lst with
        | [] -> str
        | a::rest ->
                    match a with 
                    | Black -> build rest (str + "b")
                    | White -> build rest (str + "w")
                    | Unknown -> build rest (str + ".")
                    
    build v ""

type Pattern = 
|BlackP 
|WhiteP 
|UnknownP
|ZeroOrMore of  Pattern
|OneOrMore of Pattern
|Exactly of (int * Pattern)
|FewerThan of (int * Pattern)
|Sequence of Pattern List
|Either of (Pattern * Pattern)
|Anything 
|EndOfCells 

let patternMatch (pattern:Pattern)  (cells:Cell list) = 
    let ptn i = 
        match cells with
        |[] -> None
        |_ ->
            match (List.head cells = i) with
            | false -> None
            | true -> Some [i]
     
    let Zptn i n tp =
        match cells with
        |[] -> Some []
        |_ ->
            match (List.head cells = i) with
            | false -> Some []
            | true -> 
                    let rec chk p (acc:List<Cell>) =
                        match p with
                        | [] ->
                                match tp with
                                |"exptn" -> 
                                        match n<=(List.length acc) with
                                        |true ->
                                                let lst =  List.take n acc
                                                let rec cln x xacc =
                                                        match x with 
                                                        | [] -> Some xacc
                                                        | t::rest-> 
                                                            match t with 
                                                            | i -> cln rest (List.append [i] xacc)
                                                            |_ -> None
                                                cln lst []
                                        |false -> None
                                |"Fptn" -> 
                                        match n<=(List.length acc) with
                                        |true ->
                                                let lst =  List.take n acc
                                                let rec cln x xacc =
                                                        match x with 
                                                        | [] -> Some xacc
                                                        | t::rest-> 
                                                            match t with 
                                                            | i -> cln rest (List.append [i] xacc)
                                                            |_ -> None
                                                cln lst []
                                        |false ->   let lst = acc
                                                    let rec cln x xacc =
                                                            match x with 
                                                            | [] -> Some xacc
                                                            | t::rest-> 
                                                                match t with 
                                                                | i -> cln rest (List.append [i] xacc)
                                                                |_ -> None
                                                    cln lst []
                                |_ ->
                                        let lst = acc
                                        let rec cln x xacc =
                                                match x with 
                                                | [] -> Some xacc
                                                | t::rest-> 
                                                    match t with 
                                                    | i -> cln rest (List.append [i] xacc)
                                                    |_ -> None
                                        cln lst []
                        | a::rest -> 
                                match a=i with
                                |true -> chk rest (List.append [i] acc) 
                                |false -> 
                                        match tp with
                                        |"exptn" -> 
                                                match n<=(List.length acc) with
                                                |true ->
                                                        let lst =  List.take n acc
                                                        let rec cln x xacc =
                                                                match x with 
                                                                | [] -> Some xacc
                                                                | t::rest-> 
                                                                    match t with 
                                                                    | i -> cln rest (List.append [i] xacc)
                                                                    |_ -> None
                                                        cln lst []
                                                |false -> None
                                        |"Fptn" -> 
                                                match n<=(List.length acc) with
                                                |true ->
                                                        let lst =  List.take n acc
                                                        let rec cln x xacc =
                                                                match x with 
                                                                | [] -> Some xacc
                                                                | t::rest-> 
                                                                    match t with 
                                                                    | i -> cln rest (List.append [i] xacc)
                                                                    |_ -> None
                                                        cln lst []
                                                |false ->   let lst = acc
                                                            let rec cln x xacc =
                                                                    match x with 
                                                                    | [] -> Some xacc
                                                                    | t::rest-> 
                                                                        match t with 
                                                                        | i -> cln rest (List.append [i] xacc)
                                                                        |_ -> None
                                                            cln lst []
                                        |_ ->
                                                let lst = acc
                                                let rec cln x xacc =
                                                        match x with 
                                                        | [] -> Some xacc
                                                        | t::rest-> 
                                                            match t with 
                                                            | i -> cln rest (List.append [i] xacc)
                                                            |_ -> None
                                                cln lst []
                    chk cells []
    let Optn i n =
        match cells with
        |[] -> None
        |_ -> 
            match (List.head cells = i) with
            | false -> None
            | true -> Zptn i n "optn"
    let Exptn i n =
        match cells with
        |[] -> Some []
        |_ -> 
            match (n=0) with
            | true -> Some []
            | false -> Zptn i n "exptn"
    let Fptn i n =
        match cells with
        |[] -> Some []
        |_ -> 
            match (n=0) with
            | true -> None
            | false -> Zptn i n "Fptn"
    let Sptn i = 
        let ver = []
        let ver = ver@i
        let rec mtch ptn p acc= 
                match ptn with
                |[] -> 
                        let list = acc
                        match (List.length list)>0 with 
                        |true ->
                                let lst = acc
                                let rec cln x xacc =
                                        match x with 
                                        | [] -> Some xacc
                                        | t::rest-> 
                                            match t with 
                                            | i -> cln rest (List.append [i] xacc)
                                            |_ -> None
                                cln lst []
                        |false -> 
                                 match List.length list=0 && List.length ver=0 with
                                 | true -> Some []
                                 | false -> None
                |a::rest -> match a with
                            |BlackP -> 
                                    match p with
                                    |a::prest -> 
                                                match a with
                                                |Black -> mtch rest prest (List.append [Black] acc)
                                                |_ -> None
                            |WhiteP -> 
                                    match p with
                                    |a::prest -> 
                                                match a with
                                                |White -> mtch rest prest (List.append [White] acc)
                                                |_ -> None
                            |UnknownP ->
                                    match p with
                                    |a::prest -> 
                                                match a with
                                                |Unknown -> mtch rest prest (List.append [Unknown] acc)
                                                |_ -> None
                
        mtch i cells []

    let Eptn i ii =
        match cells with
        |[] ->None
        |a::rest -> 
                match a with 
                |Black ->  
                        match i=BlackP with
                        |true -> Some [Black]
                        |false -> match ii=BlackP with
                                    |true -> Some [Black]
                                    |false -> None
                |White -> match i=WhiteP with
                        |true -> Some [White]
                        |false -> match ii=WhiteP with
                                    |true -> Some [White]
                                    |false -> None
                |Unknown -> match i=UnknownP with
                        |true -> Some [Unknown]
                        |false -> match ii=UnknownP with
                                    |true -> Some [Unknown]
                                    |false -> None
                |_ -> None
 
                    

    match pattern with
    |BlackP -> ptn Black
    |WhiteP -> ptn White
    |UnknownP -> ptn Unknown
    |ZeroOrMore a -> 
                    match a with
                    |BlackP -> Zptn Black 1 "zptn"
                    |WhiteP -> Zptn White 1 "zptn"
                    |UnknownP-> Zptn Unknown 1 "zptn"
    |OneOrMore  b -> 
                    match b with
                    |BlackP -> Optn Black 1
                    |WhiteP -> Optn White  1
                    |UnknownP-> Optn Unknown 1
    |Exactly  (c,d)  -> 
                    match d with
                    |BlackP -> Exptn Black c
                    |WhiteP -> Exptn White c
                    |UnknownP-> Exptn Unknown c

    |FewerThan(e,f) ->  
                        match (e=0) with
                        | true -> None
                        | false -> 
                                    match f with
                                    |BlackP -> Fptn Black (e-1)
                                    |WhiteP -> Fptn White (e-1)
                                    |UnknownP-> Fptn Unknown (e-1)
    |Sequence  g -> Sptn g  
    |Either  (h,i) -> Eptn h i
    |Anything -> match cells.Length>0 with
                 |true -> Some [(List.head cells)]
                 |false -> None
    |EndOfCells -> match cells.Length=0 with
                |true -> Some []
                |false -> None
       
let find pattern cells = 
    let cp c = 
        match c with 
        | Black -> BlackP
        | White -> WhiteP
        | Unknown -> UnknownP // changes a cell to a pattern so they can be comparable

    let pll a =  // changes the list of cells to a list of patterns 
        let rec pt a b = 
            match a with 
            | [] -> b 
            | c :: rest -> pt rest (b @ [cp c])
        pt a []
    let ptoc c = 
        match c with 
        | BlackP -> Black
        | WhiteP -> White
        | UnknownP -> Unknown
    let bc a =
        let rec pt a b = 
            match a with 
            | [] -> b 
            | c :: rest -> pt rest (b @ [ptoc c])
        pt a []
    let that = (fun x -> x = pattern)
    let tryFind (p : 'a -> bool) (xs : 'a list) (n : int) =
            let rec find fn lst num = 
                match lst with 
                |[] -> None 
                |a::rest -> match fn a with 
                            |true -> Some ( bc [a],num) 
                            |false -> find fn rest (num + 1)
            find p xs n
    let d = pll cells
    
    match pattern with 
    | BlackP -> tryFind that d 0
    | WhiteP -> tryFind that d 0 
    | UnknownP -> tryFind that d 0  
    | ZeroOrMore (a) -> Some ((bc []), 0 )

let map func pattern cells = 
    let mtch cl = 
        match cl with
        |BlackP -> Black
        |WhiteP -> White
        |UnknownP -> Unknown

    let ptn = mtch pattern
    let rec mp lst pt acc =
        match lst with
        | [] -> acc
        | a::rest ->
                    match a=pt with
                    | true -> mp rest pt (List.append acc ((func [a])))
                    | _ -> mp rest pt (List.append acc [a])

    mp cells ptn []

