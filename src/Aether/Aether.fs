module Aether

(* Types

   Types defining lenses, prisms, and isomorphisms
   as standard pairs. These can be implemented implicitly,
   so an assembly *providing* lenses without also consuming them
   requires no dependency on Aether, just an implicit structuring. *)

/// Lens from a -> b
type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

/// Prism from a -> b
type Prism<'a,'b> = ('a -> 'b option) * ('b -> 'a -> 'a)

type Traversal<'o,'q,'p,'i,'j> = ('o -> 'q) * (('i -> 'j) -> 'o -> 'p)

type Traversal'<'o,'i> = Traversal<'o,'i seq,'o,'i,'i>

// Isomorphisms

/// Total isomorphism of a <> b
type Isomorphism<'a,'b> = ('a -> 'b) * ('b -> 'a)

/// Epimorphism of a <> b
type Epimorphism<'a,'b> = ('a -> 'b option) * ('b -> 'a)

module private Option =
    let inline toSeq opt=
        match opt with
        | Some v -> Seq.singleton v
        | None -> Seq.empty

/// Functions for using lenses to get, set, and modify values on
/// product-types, such as tuples and records.
[<RequireQualifiedAccess>]
module Lens =

    /// Get a value using a lens
    let get ((g, _): Lens<'a,'b>) =
        fun a -> g a

    /// Set a value using a lens
    let set ((_, s): Lens<'a,'b>) =
        fun b a -> s b a

    /// Modify a value using a lens
    let map ((g, s): Lens<'a,'b>) =
        fun f a -> s (f (g a)) a

    /// Converts an isomorphism into a lens
    let ofIsomorphism ((f, t): Isomorphism<'a,'b>) : Lens<'a,'b> =
        f, (fun b _ -> t b)

/// Functions for using prisms to get, set, and modify values on
/// sum-types, such as discriminated unions.
[<RequireQualifiedAccess>]
module Prism =

    /// Get a value option using a prism
    let get ((g, _): Prism<'a,'b>) =
        fun a -> g a

    /// Get a value or a default using a prism
    let getOrElse ((g, _): Prism<'a,'b>) =
        fun b a -> g a |> function | Some b -> b | _ -> b

    /// Set a value using a prism
    let set ((_, s): Prism<'a,'b>) =
        fun b a -> s b a

    /// Modify a value using a prism
    let map ((g, s): Prism<'a,'b>) =
        fun f a -> Option.map f (g a) |> function | Some b -> s b a | _ -> a

    /// Converts an epimorphism into a prism
    let ofEpimorphism ((f, t): Epimorphism<'a,'b>) : Prism<'a,'b> =
        f, (fun b _ -> t b)

[<RequireQualifiedAccess>]
module Traversal =
    let inline sequence ((g, _) : Traversal<_,_,_,_,_>) =
        g

    let inline over ((_, s) : Traversal<_,_,_,_,_>) =
        s

    let inline set (t : Traversal<_,_,_,_,_>) v =
        over t (fun _ -> v)

    let inline ofLens ((g, s) : Lens<_,_>) : Traversal'<_,_> =
        g >> Seq.singleton,
        (fun f a -> s (f (g a)) a)

    let inline ofPrism ((g, s) : Prism<_,_>) : Traversal'<_,_> =
        g >> Option.toSeq,
        (fun f a -> Option.map f (g a) |> function | Some b -> s b a | _ -> a)

/// Functions for composing lenses, prisms, and isomorphisms, each of which
/// returns a new lens or prism based on the lenses, prisms,
/// or isomorphisms composed.
///
/// Open `Aether.Operators` to use the infix operator forms of these compositions,
/// which is significantly less verbose.
[<RequireQualifiedAccess>]
module Compose =

    /// Compose a lens with a lens, giving a lens
    let lensWithLens ((g1, s1): Lens<'a,'b>) ((g2, s2): Lens<'b,'c>) : Lens<'a,'c> =
        (fun a -> g2 (g1 a)),
        (fun c a -> s1 (s2 c (g1 a)) a)

    /// Compose a lens with a prism, giving a prism
    let lensWithPrism ((g1, s1): Lens<'a,'b>) ((g2, s2): Prism<'b,'c>) : Prism<'a,'c> =
        (fun a -> g2 (g1 a)),
        (fun c a -> s1 (s2 c (g1 a)) a)

    /// Compose a prism and a lens, giving a prism
    let prismWithLens ((g1, s1): Prism<'a,'b>) ((g2, s2): Lens<'b,'c>) : Prism<'a,'c> =
        (fun a -> Option.map g2 (g1 a)),
        (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a)

    /// Compose a prism with a prism, giving a prism
    let prismWithPrism ((g1, s1): Prism<'a,'b>) ((g2, s2): Prism<'b,'c>) : Prism<'a,'c> =
        (fun a -> Option.bind g2 (g1 a)),
        (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a)

    /// Compose a lens with an isomorphism, giving a lens
    let lensWithIsomorphism ((g, s): Lens<'a,'b>) ((f, t): Isomorphism<'b,'c>) : Lens<'a,'c> =
        (fun a -> f (g a)),
        (fun c a -> s (t c) a)

    /// Compose a lens with an epimorphism, giving a prism
    let lensWithEpimorphism ((g, s): Lens<'a,'b>) ((f, t): Epimorphism<'b,'c>) : Prism<'a,'c> =
        (fun a -> f (g a)),
        (fun c a -> s (t c) a)

    /// Compose a prism with an isomorphism, giving a prism
    let prismWithIsomorphism ((g, s): Prism<'a,'b>) ((f, t): Isomorphism<'b, 'c>) : Prism<'a,'c> =
        (fun a -> Option.map f (g a)),
        (fun c a -> s (t c) a)

    /// Compose a lens with an epimorphism, giving a prism
    let prismWithEpimorphism ((g, s): Prism<'a,'b>) ((f, t): Epimorphism<'b,'c>) : Prism<'a,'c> =
        (fun a -> Option.bind f (g a)),
        (fun c a -> s (t c) a)

    /// Compose a lens with a traversal, giving a traversal
    let lensWithTraversal ((g, s) : Lens<'a,'b>) ((q, t) : Traversal'<'b,'c>) : Traversal'<'a,'c> =
        g >> q,
        (fun f a -> s (t f (g a)) a)

    /// Compose a prism with a traversal, giving a traversal
    let prismWithTraversal ((g, s) : Prism<'a,'b>) ((q, t) : Traversal'<'b,'c>) : Traversal'<'a,'c> =
        g >> Option.toSeq >> Seq.collect q,
        (fun f a -> Option.map (t f) (g a) |> function | Some b -> s b a | None -> a)

    /// Compose a traversal with a traversal, giving a traversal
    let traversalWithTraversal ((q1, t1) : Traversal'<'a,'b>) ((q2,t2) : Traversal'<'b,'c>) : Traversal'<'a,'c> =
        q1 >> Seq.collect q2,
        (fun f a -> t1 (t2 f) a)

    let traversalWithLens (t : Traversal'<'a,'b>) (l : Lens<'b,'c>) : Traversal'<'a,'c> =
        traversalWithTraversal t (Traversal.ofLens l)

    let traversalWithPrism (t : Traversal'<'a,'b>) (p : Prism<'b,'c>) : Traversal'<'a,'c> =
        traversalWithTraversal t (Traversal.ofPrism p)

/// Various optics implemented for common types such as tuples,
/// lists and maps, along with an id_ lens.
[<AutoOpen>]
module Optics =

    /// Identity lens returning the original item regardless of modification.
    /// Useful for composing a lens out of a chain of one or more isomorphisms/epimorphisms.
    let id_ : Lens<'a,'a> =
        (fun x -> x), (fun x _ -> x)

    /// Lens to the first item of a tuple
    let fst_ : Lens<('a * 'b),'a> =
        fst, (fun a t -> a, snd t)

    /// Lens to the second item of a tuple
    let snd_ : Lens<('a * 'b),'b> =
        snd, (fun b t -> fst t, b)
        // : Traversal2<'a * 'a, 'a>
        // : ('o -> seq<'i>) * (('i -> 'i) -> 'o -> 'o)

    let both_ : Traversal'<('a * 'a), 'a> =
        (fun (f,s) -> seq { yield f; yield s }),
        (fun f a -> f (fst a), f (snd a))

    [<RequireQualifiedAccess>]
    module Seq =

        let traverseWithLens (lens : Lens<'i, 'j>) : Traversal'<'i seq,'j> =
            Seq.map (Lens.get lens),
            Lens.map lens >> Seq.map

        let traverseWithPrism (prism : Prism<'i, 'j>) : Traversal'<'i seq,'j> =
            Seq.choose (Prism.get prism),
            Prism.map prism >> Seq.map

        let traverse (t : Traversal'<'i, 'j>) : Traversal'<'i seq,'j> =
            Seq.collect (Traversal.sequence t),
            Traversal.over t >> Seq.map

    [<RequireQualifiedAccess>]
    module Array =

        /// Isomorphism to an list
        let list_ : Isomorphism<'v[], 'v list> =
            Array.toList, Array.ofList

        let traverseWithLens (lens : Lens<'i, 'j>) : Traversal'<'i[],'j> =
            Array.map (Lens.get lens) >> Seq.ofArray,
            Lens.map lens >> Array.map

        let traverseWithPrism (prism : Prism<'i, 'j>) : Traversal'<'i[],'j> =
            Array.choose (Prism.get prism) >> Seq.ofArray,
            Prism.map prism >> Array.map

        let traverse (t : Traversal'<'i, 'j>) : Traversal'<'i[],'j> =
            Seq.collect (Traversal.sequence t),
            Traversal.over t >> Array.map

    [<RequireQualifiedAccess>]
    module List =

        /// Prism to the head of a list
        let head_ : Prism<'v list, 'v> =
            (function | h :: _ -> Some h | _ -> None),
            (fun v -> function | _ :: t -> v :: t | l -> l)

        /// Prism to an indexed element in a list
        let pos_ (i: int) : Prism<'v list, 'v> =
            (function | l when List.length l > i -> Some (List.nth l i) | _ -> None),
            (fun v l -> List.mapi (fun i' x -> if i = i' then v else x) l)

        /// Prism to the tail of a list
        let tail_ : Prism<'v list, 'v list> =
            (function | _ :: t -> Some t | _ -> None),
            (fun t -> function | h :: _ -> h :: t | [] -> [])

        /// Isomorphism to an array
        let array_ : Isomorphism<'v list, 'v[]> =
            List.toArray, List.ofArray

        let traverseWithLens (lens : Lens<'i, 'j>) : Traversal'<'i list,'j> =
            List.map (Lens.get lens) >> Seq.ofList,
            Lens.map lens >> List.map

        let traverseWithPrism (prism : Prism<'i, 'j>) : Traversal'<'i list,'j> =
            List.choose (Prism.get prism) >> Seq.ofList,
            Prism.map prism >> List.map

        let traverse (t : Traversal'<'i, 'j>) : Traversal'<'i list,'j> =
            Seq.collect (Traversal.sequence t),
            Traversal.over t >> List.map

    [<RequireQualifiedAccess>]
    module Map =

        /// Prism to a value associated with a key in a map
        let key_ (k: 'k) : Prism<Map<'k,'v>,'v> =
            Map.tryFind k, (fun v x -> if Map.containsKey k x then Map.add k v x else x)

        /// Lens to a value option associated with a key in a map
        let value_ (k: 'k) : Lens<Map<'k,'v>, 'v option> =
            Map.tryFind k,
            (fun v x ->
                match v with
                | Some v -> Map.add k v x
                | _ -> Map.remove k x)

        /// Weak Isomorphism to an array of key-value pairs
        let array_ : Isomorphism<Map<'k,'v>, ('k * 'v)[]> =
            Map.toArray, Map.ofArray

        /// Weak Isomorphism to a list of key-value pairs
        let list_ : Isomorphism<Map<'k,'v>, ('k * 'v) list> =
            Map.toList, Map.ofList

        let traverseWithLens (lens : Lens<'i, 'j>) : Traversal'<Map<'k,'i>,'j> =
            Map.toArray >> Array.map (fun (_,v) -> Lens.get lens v) >> Seq.ofArray,
            fun f -> Map.map (fun _ -> Lens.map lens f)

        let traverseWithPrism (prism : Prism<'i, 'j>) : Traversal'<Map<'k,'i>,'j> =
            Map.toArray >> Seq.ofArray >> Seq.choose (fun (_,v) -> Prism.get prism v),
            fun f -> Map.map (fun _ -> Prism.map prism f)

        let traverse (t : Traversal'<'i, 'j>) : Traversal'<Map<'k,'i>,'j> =
            Map.toArray >> Seq.ofArray >> Seq.collect (fun (_,v) -> Traversal.sequence t v),
            fun f -> Map.map (fun _ -> Traversal.over t f)

    [<RequireQualifiedAccess>]
    module Set =

        let traverseWithLens (lens : Lens<'i, 'j>) : Traversal'<Set<'i>,'j> =
            Set.toArray >> Array.map (Lens.get lens) >> Seq.ofArray,
            Lens.map lens >> Set.map

        let traverseWithPrism (prism : Prism<'i, 'j>) : Traversal'<Set<'i>,'j> =
            Set.toArray >> Seq.ofArray >> Seq.choose (Prism.get prism),
            Prism.map prism >> Set.map

        let traverse (t : Traversal'<'i, 'j>) : Traversal'<Set<'i>,'j> =
            Set.toArray >> Seq.ofArray >> Seq.collect (Traversal.sequence t),
            Traversal.over t >> Set.map

    [<RequireQualifiedAccess>]
    module Option =

        /// Prism to the value in an Option
        let value_ : Prism<'v option, 'v> =
            id, (fun v -> function | Some _ -> Some v | None -> None)

    [<RequireQualifiedAccess>]
    module Choice =

        /// Prism to Choice1Of2
        let choice1Of2_ : Prism<Choice<_,_>, _> =
            ((fun x -> match x with | Choice1Of2 v -> Some v | _ -> None),
             (fun v x -> match x with | Choice1Of2 _ -> Choice1Of2 v | _ -> x))

        /// Prism to Choice2Of2
        let choice2Of2_ : Prism<Choice<_,_>, _> =
            ((fun x -> match x with | Choice2Of2 v -> Some v | _ -> None),
             (fun v x -> match x with | Choice2Of2 _ -> Choice2Of2 v | _ -> x))

/// Optional custom operators for composing optics. Provided as syntactic
/// alternatives to more verbose composition functions in `Aether.Compose`.
module Operators =

    /// Compose a lens with a lens, giving a lens
    let inline (>-->) l1 l2 =
        Compose.lensWithLens l1 l2

    /// Compose a lens and a prism, giving a prism
    let inline (>-?>) l1 l2 =
        Compose.lensWithPrism l1 l2

    /// Compose a prism and a lens, giving a prism
    let inline (>?->) l1 l2 =
        Compose.prismWithLens l1 l2

    /// Compose a prism with a prism, giving a prism
    let inline (>??>) l1 l2 =
        Compose.prismWithPrism l1 l2

    let inline (>%%>) t1 t2 =
        Compose.traversalWithTraversal t1 t2

    let inline (>-%>) l t =
        Compose.lensWithTraversal l t

    let inline (>?%>) p t =
        Compose.prismWithTraversal p t

    let inline (>%->) t l =
        Compose.traversalWithLens t l

    let inline (>%?>) t p=
        Compose.traversalWithPrism t p

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

    (* Function Operators

       Operators as infix alternatives to some of the standard get, set,
       modify functions (getL, setL, etc.) Should likely be used rather
       sparingly and in specific controlled areas unless you're aiming for
       symbol soup. *)

    /// Get a value using a lens
    let inline (^.) (a: 'a) (l: Lens<'a,'b>) : 'b =
        Lens.get l a

    /// Get a value using a prism
    let inline (^?.) (a: 'a) (l: Prism<'a,'b>) : 'b option =
        Prism.get l a

    let inline (^..) (a: 'a) (t: Traversal'<'a,'b>) : 'b seq =
        Traversal.sequence t a

    /// Set a value using a lens
    let inline (^=) (b: 'b) (l: Lens<'a,'b>) : 'a -> 'a =
        Lens.set l b

    /// Set a value using a prism
    let inline (^?=) (b: 'b) (l: Prism<'a,'b>) : 'a -> 'a =
        Prism.set l b

    let inline (^%~) (b: 'b) (t: Traversal'<'a,'b>) =
        Traversal.over t (fun _ -> b)

    /// Modify a value using a lens
    let inline (^%=) (f: 'b -> 'b) (l: Lens<'a,'b>) : 'a -> 'a =
        Lens.map l f

    /// Modify a value using a prism
    let inline (^?%=) (f: 'b -> 'b) (l: Prism<'a,'b>) : 'a -> 'a =
        Prism.map l f

    let inline (^%%) (f: 'b -> 'b) (t: Traversal'<'a,'b>) =
        Traversal.over t f

