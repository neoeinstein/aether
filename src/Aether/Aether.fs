module Aether

(* Types

   Types defining lenses, prisms, and isomorphisms
   as standard pairs. These can be implemented implicitly,
   so an assembly *providing* lenses without also consuming them
   requires no dependency on Aether, just an implicit structuring. *)

type Getter<'s,'a> = ('s -> 'a)

type Setter<'s,'t,'a,'b> = (('a -> 'b) -> 's -> 't)
type Setter<'s,'a> = Setter<'s,'s,'a,'a>
type Setter'<'s,'t,'a> = ('a -> 's -> 't)
type Setter'<'s,'a> = Setter'<'s,'s,'a>

type Traversal<'s,'t,'a,'b> = Setter<'s,'t,'a,'b>
type Traversal<'s,'a> = Traversal<'s,'s,'a,'a>

type Review<'s,'a> = ('a -> 's)

type Optic<'s,'t,'g,'a,'b> = Getter<'s,'g> * Traversal<'s,'t,'a,'b>

/// Lens from s -> a
type Lens<'s,'t,'a,'b> = Getter<'s,'a> * Traversal<'s,'t,'a,'b>
type Lens<'s,'a> = Lens<'s,'s,'a,'a>

/// Prism from a -> b
type Prism<'s,'t,'a,'b> = Getter<'s,'a option> * Traversal<'s,'t,'a,'b>
type Prism<'s,'a> = Prism<'s,'s,'a,'a>

type Multilens<'s,'t,'a,'b> = Getter<'s,'a seq> * Traversal<'s,'t,'a,'b>
type Multilens<'s,'a> = Multilens<'s,'s,'a,'a>

// Isomorphisms

/// Total isomorphism of a <> b
type Isomorphism<'s,'t,'a,'b> = Getter<'s,'a> * Review<'t,'b>
type Isomorphism<'s,'a> = Isomorphism<'s,'s,'a,'a>

/// Epimorphism of a <> b
type Epimorphism<'a,'b> = ('a -> 'b option) * ('b -> 'a)

module Option =
    let apply (f : ('a -> 'b) option) (x : 'a option) : 'b option =
        match f with
        | Some g ->
            match x with
            | Some y -> g y |> Some
            | _ -> None
        | _ -> None

[<RequireQualifiedAccess>]
module Getter =
    let inline get (g : Getter<'s,'a>) =
        g

    let inline ofSingleton (g : Getter<'s,'a>) =
        g >> Seq.singleton

    let inline ofOption (g : Getter<'s,'a option>) =
        g >> function | Some v -> Seq.singleton v | None -> Seq.empty

    let inline toSingleton (g : Getter<'s,'a seq>) =
        g >> Seq.exactlyOne

    let inline toOption (g : Getter<'s,'a seq>) =
        g >> (fun q -> if Seq.exists (fun _ -> true) q then Some <| Seq.head q else None)

[<RequireQualifiedAccess>]
module Setter =
    let inline over (s : Setter<'s,'t,'a,'b>) map =
        s map

    let inline set (s : Setter<'s,'t,'a,'b>) value =
        over s (fun _ -> value)

module Traversal =
    let inline over (t : Traversal<'s,'t,'a,'b>) =
        t

    let inline set (t : Traversal<'s,'t,'a,'b>) (value : 'b) =
        over t (fun _ -> value)

    let (>>) (t1 : Traversal<'s,'t,'a,'b>) (t2 : Traversal<'x,'y,'s,'t>) : Traversal<'x,'y,'a,'b> =
        t1 >> t2

[<RequireQualifiedAccess>]
module Optics =
    let inline get ((getter, _): Optic<'s,'t,'g,'a,'b>) =
        Getter.get getter

    /// Set a value using a lens
    let inline set ((_, setter): Optic<'s,'t,'g,'a,'b>) : Setter'<'s,'t,'b> =
        Setter.set setter

    /// Modify a value using a lens
    let inline map ((_, setter): Optic<'s,'t,'g,'a,'b>) : Setter<'s,'t,'a,'b> =
        Setter.over setter

    let compose ((g1,s1) : Optic<'s,'t,'g,'a,'b>) ((g2,s2) : Optic<'x,'y,'f,'s,'t>) (glue : Traversal<_,_,_,_>) =
        g2 >> glue g1,
        s2 << s1

    let inline composeOptic (o1: Optic<'s,'t,#seq<'a>,'a,'b>) (o2: Optic<'x,'y,#seq<'s>,'s,'t>) : Multilens<'x,'y,'a,'b> =
        compose o1 o2 Seq.collect

/// Functions for using lenses to get, set, and modify values on
/// product-types, such as tuples and records.
[<RequireQualifiedAccess>]
module Lens =

    let inline ofGetSet (getter : Getter<'s,'a>) (setter : Setter'<'s,'t,'b>) : Lens<'s,'t,'a,'b> =
        getter, fun f s -> setter (f (getter s)) s

    let inline composeLens (l1: Lens<'s,'t,'a,'b>) (l2: Lens<'x,'y,'s,'t>) : Lens<'x,'y,'a,'b> =
        Optics.compose l1 l2 id

    let inline composePrism (l: Lens<'s,'t,'a,'b>) (p: Prism<'x,'y,'s,'t>) : Prism<'x,'y,'a,'b> =
        Optics.compose l p Option.map

    let inline composeOptic (l: Lens<'s,'t,'a,'b>) (o: Optic<'x,'y,#seq<'s>,'s,'t>) : Multilens<'x,'y,'a,'b> =
        Optics.compose l o Seq.collect

    /// Converts an isomorphism into a lens
    let inline ofIsomorphism ((t, f): Isomorphism<'s,'a>) : Lens<'s,'a> =
        t, fun map -> t >> map >> f

/// Functions for using prisms to get, set, and modify values on
/// sum-types, such as discriminated unions.
[<RequireQualifiedAccess>]
module Prism =

    let inline ofGetSetMap (getter : Getter<'s,'a option>) (setter : Setter'<'s,'t,'b>) (map : 's -> 't) : Prism<'s,'t,'a,'b> =
        getter,
        fun f s ->
            match getter s with
            | Some a -> setter (f a) s
            | None -> map s

    let inline ofGetSet g s =
        ofGetSetMap g s id

    let inline composeLens (p: Prism<'s,'t,'a,'b>) (l: Lens<'x,'y,'s,'t>) : Prism<'x,'y,'a,'b> =
        Optics.compose p l id

    let inline composePrism (p1: Prism<'s,'t,'a,'b>) (p2: Prism<'x,'y,'s,'t>) : Prism<'x,'y,'a,'b> =
        Optics.compose p1 p2 Option.bind

    let inline composeOptic (p: Prism<'s,'t,'a,'b>) (o: Optic<'x,'y,#seq<'s>,'s,'t>) : Multilens<'x,'y,'a,'b> =
        Optics.compose p o (fun f -> Seq.collect (f >> function | Some v -> Seq.singleton v | None -> Seq.empty))

[<RequireQualifiedAccess>]
module Multilens =
        let inline composeLens (l1: Multilens<'s,'t,'a,'b>) (l2: Lens<'x,'y,'s,'t>) : Multilens<'x,'y,'a,'b> =
            Optics.compose l1 l2 id

        let inline composePrism (l: Multilens<'s,'t,'a,'b>) (p: Prism<'x,'y,'s,'t>) : Multilens<'x,'y,'a,'b> =
            Optics.compose l p (fun f -> function | Some v -> f v | None -> Seq.empty)

        let inline composeOptic (l: Multilens<'s,'t,'a,'b>) (o: Optic<'x,'y,#seq<'s>,'s,'t>) : Multilens<'x,'y,'a,'b> =
            Optics.compose l o Seq.collect

/// Various optics implemented for common types such as tuples,
/// lists and maps, along with an id_ lens.
[<AutoOpen>]
module Core =

    /// Identity lens returning the original item regardless of modification.
    /// Useful for composing a lens out of a chain of one or more isomorphisms/epimorphisms.
    let id_ : Lens<'a,'a> =
        id, id

    [<RequireQualifiedAccess>]
    module Seq =
        let each_ : Multilens<'i seq,'i> =
            id, Seq.map

    [<RequireQualifiedAccess>]
    module Array =
        type Multilens<'s,'t,'a,'b> = Getter<'s,'a[]> * Traversal<'s,'t,'a,'b>
        type Multilens<'s,'a> = Multilens<'s,'s,'a,'a>

        let each_ : Multilens<'i[],'i> =
            id, Array.map

        /// Isomorphism to an list
        let list_ : Isomorphism<'v[], 'v list> =
            Array.toList, Array.ofList

    [<RequireQualifiedAccess>]
    module List =
        type Multilens<'s,'t,'a,'b> = Getter<'s,'a list> * Traversal<'s,'t,'a,'b>
        type Multilens<'s,'a> = Multilens<'s,'s,'a,'a>

        let each_ : Multilens<'i list,'i> =
            id, List.map

        /// Prism to the head of a list
        let head_ : Prism<'v list, 'v> =
            (function | h :: _ -> Some h | _ -> None),
            (fun f -> function | h :: t -> f h :: t | l -> l)

        /// Prism to the tail of a list
        let tail_ : Prism<'v list, 'v list> =
            (function | _ :: t -> Some t | _ -> None),
            (fun f -> function | h :: t -> h :: f t | [] -> [])

        /// Prism to an indexed element in a list
        let rec pos_ (i: int) : Prism<'v list, 'v> =
            match i with
            | 0 -> head_
            | _ -> Prism.composePrism (pos_ (i - 1)) tail_

        /// Prism to an indexed element in a list
        let posOld_ (i: int) : Prism<'v list, 'v> =
            (function | l when List.length l > i -> Some l.[i] | _ -> None),
            (fun f l -> List.mapi (fun i' x -> if i = i' then f x else x) l)

        /// Isomorphism to an array
        let array_ : Isomorphism<'v list, 'v[]> =
            List.toArray, List.ofArray

    [<RequireQualifiedAccess>]
    module Map =
        let each_ : Multilens<Map<'k,'v>,Map<'k,'v>,'k*'v,'v> =
            Map.toSeq,
            fun f m -> Map.map (fun k v -> f (k,v)) m

        /// Prism to a value associated with a key in a map
        let key_ (k: 'k) : Prism<Map<'k,'v>,'v> =
            Map.tryFind k,
            fun f x ->
                Map.tryFind k x
                |> function
                    | Some v -> Map.add k (f v) x
                    | None -> x

        /// Lens to a value option associated with a key in a map
        let value_ (k: 'k) : Lens<Map<'k,'v>, 'v option> =
            Map.tryFind k,
            fun f x ->
                Map.tryFind k x
                |> f
                |> function
                    | Some v -> Map.add k v x
                    | None -> Map.remove k x

        /// Weak Isomorphism to an array of key-value pairs
        let array_ : Isomorphism<Map<'k,'v>, ('k * 'v)[]> =
            Map.toArray, Map.ofArray

        /// Weak Isomorphism to a list of key-value pairs
        let list_ : Isomorphism<Map<'k,'v>, ('k * 'v) list> =
            Map.toList, Map.ofList

    [<RequireQualifiedAccess>]
    module Set =
        let each_ : List.Multilens<Set<'i>,'i> =
            Seq.toList, Set.map

    [<RequireQualifiedAccess>]
    module Tuple =
        /// Lens to the first item of a tuple
        let fst_ : Lens<('l * 'r),'l> =
            fst, (fun f (l,r) -> f l, r)

        /// Lens to the second item of a tuple
        let snd_ : Lens<('l * 'r),'r> =
            snd, (fun f (l,r) -> l, f r)

        let both_ : Multilens<('a * 'a), 'a> =
            (fun (f,s) -> [|f;s|] |> Array.toSeq),
            (fun f a -> f (fst a), f (snd a))

    [<RequireQualifiedAccess>]
    module Option =

        /// Prism to the value in an Option
        let value_ : Prism<'v option, 'v> =
            id, Option.map

    [<RequireQualifiedAccess>]
    module Choice =

        /// Prism to Choice1Of2
        let choice1Of2_ : Prism<Choice<_,_>, _> =
            (function | Choice1Of2 v -> Some v | _ -> None),
            (fun f -> function | Choice1Of2 v -> Choice1Of2 (f v) | x -> x)

        /// Prism to Choice2Of2
        let choice2Of2_ : Prism<Choice<_,_>, _> =
            (function | Choice2Of2 v -> Some v | _ -> None),
            (fun f -> function | Choice2Of2 v -> Choice2Of2 (f v) | x -> x)

/// Optional custom operators for composing optics. Provided as syntactic
/// alternatives to more verbose composition functions in `Aether.Compose`.
module Operators =

    /// Compose a lens with a lens, giving a lens
    let inline (>-->) l1 l2 =
        Lens.composeLens l1 l2

    /// Compose a lens and a prism, giving a prism
    let inline (>-?>) l p =
        Lens.composePrism l p

    /// Compose a prism and a lens, giving a prism
    let inline (>?->) p l =
        Prism.composeLens p l

    /// Compose a prism with a prism, giving a prism
    let inline (>??>) p1 p2 =
        Prism.composePrism p1 p2

    let inline (>%%>) o1 o2 =
        Multilens.composeOptic o1 o2

    let inline (>-%>) l o =
        Lens.composeOptic l o

    let inline (>?%>) p o =
        Prism.composeOptic p o

    let inline (>%->) m l =
        Multilens.composeLens m l

    let inline (>%?>) m p =
        Multilens.composePrism m p
(*
    /// Compose a lens with an isomorphism, giving a total lens
    let inline (<-->) l i =
        Compose.lensWithIsomorphism l i

    /// Compose a total lens with an epimorphism, giving a prism
    let inline (<-?>) l i =
        Compose.lensWithEpimorphism l i

    /// Compose a prism with an isomorphism, giving a prism
    let inline (<?->) l i =
        Compose.prismWithIsomorphism l i

    /// Compose a prism with an epimorphism, giving a prism
    let inline (<??>) l i =
        Compose.prismWithEpimorphism l i
*)
    (* Function Operators

       Operators as infix alternatives to some of the standard get, set,
       modify functions (getL, setL, etc.) Should likely be used rather
       sparingly and in specific controlled areas unless you're aiming for
       symbol soup. *)

    /// Get a value using a lens
    let inline (^.) (a: 'a) (l: Lens<'a,'b>) : 'b =
        Optics.get l a

    /// Set a value using a lens
    let inline (^=) (b: 'b) (l: Lens<'a,'b>) : 'a -> 'a =
        Optics.set l b

    /// Modify a value using a lens
    let inline (^%=) (f: 'b -> 'b) (l: Lens<'a,'b>) : 'a -> 'a =
        Optics.map l f

(*
let cho i (v : int) = match i % 2 with | 0 -> Choice1Of2 v | _ -> string v |> Choice2Of2

let s2 m = Seq.initInfinite (fun i -> (((cho i i),(cho (i + 1) (i * m))), (string i,string (i * m))))

let s = s2 2

open Operators

// Need to reverse composition functions

let i = Optics.get (Multilens.composeOptic (Multilens.composeLens (Prism.composeOptic Choice.choice1Of2_ Tuple.both_) Tuple.fst_) Seq.each_) (s2 2);;

let i = Optics.map (Choice.choice1Of2_ >?%> Tuple.both_ >%-> Tuple.fst_ >%%> Seq.each_) (~-) (s2 2);;

let j = Optics.map (Tuple.both_ >%-> Tuple.snd_ >%%> Seq.each_) (fun s -> "_" + s + "_") i;;

*)
