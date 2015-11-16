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

type Traversable<'s,'t,'a,'b> = 's * Traversal<'s,'t,'a,'b>

type Review<'b,'t> = ('b -> 't)

type Optic<'s,'t,'g,'a,'b> = Getter<'s,'g> * Traversal<'s,'t,'a,'b>
type Optic<'s,'g,'a> = Optic<'s,'s,'g,'a,'a>

/// Lens from s -> a
type Lens<'s,'t,'a,'b> = Getter<'s,'a> * Traversal<'s,'t,'a,'b>
type Lens<'s,'a> = Lens<'s,'s,'a,'a>

/// Prism from a -> b
type Prism<'s,'t,'a,'b> = Review<'b,'t> * Getter<'s,Choice<'a,'t>>
type Prism<'s,'a> = Prism<'s,'s,'a,'a>

type Multilens<'s,'t,'a,'b> = Getter<'s,'a seq> * Traversal<'s,'t,'a,'b>
type Multilens<'s,'a> = Multilens<'s,'s,'a,'a>

// Isomorphisms

/// Total isomorphism of a <> b
type Isomorphism<'s,'t,'a,'b> = Getter<'s,'a> * Review<'t,'b>
type Isomorphism<'s,'a> = Isomorphism<'s,'s,'a,'a>

/// Epimorphism of a <> b
type Epimorphism<'a,'b> = ('a -> 'b option) * ('b -> 'a)

let inline always x =
    fun _ -> x

module Option =
    let either f def = function
        | Some v -> f v
        | None -> def

    let apply (f : ('a -> 'b) option) (x : 'a option) : 'b option =
        match f with
        | Some g ->
            match x with
            | Some y -> g y |> Some
            | _ -> None
        | _ -> None

module Choice =
    let bimap g h = function
        | Choice1Of2 x -> g x
        | Choice2Of2 x -> h x

    let inline map f = bimap f f

//    let dimap (ca : 'c -> Choice<'a,'a>) (bd : Choice<'b,'b> -> 'd) (f: 'a -> 'b) =
//        ca >> map f >> bd

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

    let alongsideWith (glue : Traversal<_,_,_,_>) ((g1,s1) : Optic<'s,'t,'f,'x,'y>) ((g2,s2) : Optic<'x,'y,'g,'a,'b>) =
        g1 >> glue g2,
        s1 << s2

    let alongside ((g1,s1) : Optic<'s,'t,'x,'x,'y>) ((g2,s2) : Optic<'x,'y,'g,'a,'b>) =
        g1 >> g2,
        s1 << s2

    let inline each (o1: Optic<'s,'t,#seq<'x>,'x,'y>) (o2: Optic<'x,'y,#seq<'a>,'a,'b>) : Optic<'s,'t,seq<'a>,'a,'b> =
        alongsideWith Seq.collect o1 o2

/// Functions for using lenses to get, set, and modify values on
/// product-types, such as tuples and records.
[<RequireQualifiedAccess>]
module Lens =

    let inline mk (getter : Getter<'s,'a>) (setter : Setter'<'s,'t,'b>) : Lens<'s,'t,'a,'b> =
        getter, fun f s -> setter (f (getter s)) s

    /// Converts an isomorphism into a lens
    let inline ofIsomorphism ((t, f): Isomorphism<'s,'a>) : Lens<'s,'a> =
        t, fun map -> t >> map >> f

/// Functions for using prisms to get, set, and modify values on
/// sum-types, such as discriminated unions.
[<RequireQualifiedAccess>]
module Prism =

    let inline mk (review : Review<'b,'t>) (getter : Getter<'s,Choice<'a,'t>>) : Prism<'s,'t,'a,'b> =
        review, getter

    let inline mk' (review : Review<'b,'s>) (getter : Getter<'s,'a option>) : Prism<'s,'s,'a,'b> =
        review,
        (fun s -> getter s |> function | Some a -> Choice1Of2 a | None -> Choice2Of2 s)

    let toOptic ((review, getter) : Prism<'s,'t,'a,'b>) : Optic<'s,'t,'a option,'a,'b> =
        (getter >> function | Choice1Of2 a -> Some a | _ -> None),
        (fun f -> getter >> function | Choice1Of2 a -> (f >> review) a | Choice2Of2 t -> t)

    let review ((review,_) : Prism<'s,'t,'a,'b>) b =
        review b

    let matching ((_,getter) : Prism<'s,'t,'a,'b>) (s: 's) =
        getter s

    let over ((review,getter) : Prism<'s,'t,'a,'b>) (f : 'a -> 'b) =
        getter
        >> function
            | Choice1Of2 a -> (f >> review) a
            | Choice2Of2 t -> t

    let preview (p : Prism<'s,'t,'a,'b>) (b : 'b) =
        over p (fun _ -> b)

    let without ((bt,seta) : Prism<'s,'t,'a,'b>) ((dv,uevc) : Prism<'u,'v,'c,'d>) : Prism<Choice<'s,'u>,Choice<'t,'v>,Choice<'a,'c>,Choice<'b,'d>> =
        Choice.bimap (bt >> Choice1Of2) (dv >> Choice2Of2),
        (fun su ->
            match su with
            | Choice1Of2 s -> Choice.bimap (Choice1Of2 >> Choice1Of2) (Choice1Of2 >> Choice2Of2) (seta s)
            | Choice2Of2 u -> Choice.bimap (Choice2Of2 >> Choice1Of2) (Choice2Of2 >> Choice2Of2) (uevc u))

    let isnt (p : Prism<'s,'t,'a,'b>) (s: 's) =
        match matching p s with
        | Choice1Of2 _ -> false
        | Choice2Of2 _ -> true

//    let below ((bt,seta) : Prism<'s,'s,'a,'a>) ((lift,traverse) : Traversable<'fs,'fs,'a,'a>) : Prism<'fs,'fs,'a,'a> =
//        lift bt,
//        (fun s ->
//            match traverse seta s with
//            | Choice1Of2 t -> Choice1Of2 t
//            | Choice2Of2 _ -> Choice2Of2 s)

    let toOptic ((_, getter) as p : Prism<'s,'t,'a,'b>) : Optic<'s,'t,'a option,'a,'b> =
        (getter >> function | Choice1Of2 a -> Some a | _ -> None),
        over p

    let bindSeq = function
        | Some v -> Seq.singleton v
        | None -> Seq.empty

    let bindArray = function
        | Some v -> [|v|]
        | None -> [||]

    let bindList = function
        | Some v -> [v]
        | None -> []

[<RequireQualifiedAccess>]
module Multilens =
        let inline composeLens (o: Optic<'s,'t,#seq<'x>,'x,'y>) (c: Lens<'x,'y,'a,'b>) : Multilens<'s,'t,'a,'b> =
            Optics.compose Seq.collect o c

        let inline composePrism (o: Optic<'s,'t,#seq<'x>,'x,'y>) (c: Prism<'x,'y,'a,'b>) glue: Multilens<'s,'t,'a,'b> =
            Optics.compose (fun f -> Seq.collect (f >> glue)) o c

        let inline composeOptic (o: Optic<'s,'t,#seq<'x>,'x,'y>) (c: Optic<'x,'y,#seq<'a>,'a,'b>) : Multilens<'s,'t,'a,'b> =
            Optics.compose Seq.collect o c


/// Various optics implemented for common types such as tuples,
/// lists and maps, along with an id_ lens.
[<AutoOpen>]
module Core =

    /// Identity lens returning the original item regardless of modification.
    /// Useful for composing a lens out of a chain of one or more isomorphisms/epimorphisms.
    let id_ : Lens<'a,'a> =
        id, id

    let void_ : Prism<'s,'s,'a,unit> =
        Prism.mk (fun () -> Unchecked.defaultof<'s>) Choice2Of2

    let only_ a : Prism<'a,unit> =
        Prism.mk' (always a) (fun s -> if s = a then Some () else None)

    let nearly_ a p : Prism<'a,unit> =
        Prism.mk' (always a) (fun s -> if p s then Some () else None)

    let show_ tryParse : Prism<string,'a> =
        Prism.mk string (fun s -> match tryParse s with | Choice1Of2 a -> Choice1Of2 a | Choice2Of2 _ -> Choice2Of2 s)

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

        let each_ : Optic<'i list,'i list,'i seq,'i,'i> =
            List.toSeq, List.map

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
            (fun l -> if i < List.length l then Some l.[i] else None),
            (fun f -> List.mapi (fun j -> if i = j then f else id))

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

        let both_ : Optic<('a * 'a),('a * 'a),'a[],'a,'a> =
            (fun (f,s) -> [|f;s|]),
            (fun f a -> f (fst a), f (snd a))

    [<RequireQualifiedAccess>]
    module Option =

        let some_ : Prism<'v option, 'v> =
            Prism.mk
                Some
                (Option.either Choice1Of2 (Choice2Of2 None))

        let none_ : Prism<'v option, unit> =
            Prism.mk'
                (always None)
                (Option.either (always None) (Some ()))

    [<RequireQualifiedAccess>]
    module Choice =

        /// Prism to Choice1Of2
        let choice1Of2_ : Prism<Choice<'a,'b>, 'a> =
            Prism.mk
                Choice1Of2
                (Choice.bimap Choice1Of2 (Choice2Of2 >> Choice2Of2))

        /// Prism to Choice2Of2
        let choice2Of2_ : Prism<Choice<'a,'b>, 'b> =
            Prism.mk
                Choice2Of2
                (Choice.bimap (Choice1Of2 >> Choice2Of2) Choice1Of2)

/// Optional custom operators for composing optics. Provided as syntactic
/// alternatives to more verbose composition functions in `Aether.Compose`.
module Operators =

    /// Compose a lens with a lens, giving a lens
    let inline (^>-->) o c =
        Lens.composeLens o c

    let inline (<--<) c o =
        Lens.composeLens o c

    /// Compose a lens and a prism, giving a prism
    let inline (^>-?>) o c =
        Lens.composePrism o c

    let inline (<?-<) c o =
        Lens.composePrism o c

    /// Compose a prism and a lens, giving a prism
    let inline (^>?->) o c =
        Prism.composeLens o c

    let inline (<-?<) c o =
        Prism.composeLens o c

    /// Compose a prism with a prism, giving a prism
    let inline (^>??>) o c =
        Prism.composePrism o c

    let inline (<??<) c o =
        Prism.composePrism o c

    let inline (^>%%>) o c =
        Optics.composeOptic o c

    let inline (<%%<) c o =
        Optics.composeOptic o c

    let inline (^>-%>) o c =
        Lens.composeOptic o c

    let inline (<%-<) c o =
        Lens.composeOptic o c

    let inline (^>?%>) o (c,def) =
        Prism.composeOptic o c def

    let inline (<%?<) (c,def) o =
        Prism.composeOptic o c def

    let inline (^>%->) o c =
        Multilens.composeLens o c

    let inline (<-%<) c o =
        Multilens.composeLens o c

    let inline (^>%?>) (o,glue) c =
        Multilens.composePrism o c glue

    let inline (<?%<) c (o,glue) =
        Multilens.composePrism o c glue
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
    let inline (^.) (s: 's) (o: Optic<'s,'t,'g,'a,'b>) : 'g =
        Optics.get o s

    /// Set a value using a lens
    let inline (^~) (s: 's) (o: Optic<'s,'t,'g,'a,'b>) (b: 'b) : 't =
        Optics.set o b s

    /// Modify a value using a lens
    let inline (^%) (s: 's) (o: Optic<'s,'t,'g,'a,'b>) (f: 'a -> 'b) : 't =
        Optics.map o f s
