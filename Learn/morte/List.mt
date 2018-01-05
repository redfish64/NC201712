-- let data List a = Cons a (List a) | Nil
--
-- in  result
( λ(result : *) =>
( λ(Items : *) =>
( λ(i1 : Items) =>
( λ(i2 : Items) =>
( λ(VItems : *) =>
( λ(vi1 : VItems) =>
( λ(vi2 : VItems) =>

(   λ(List : * -> *)
=>  λ(Cons : forall (a : *) -> a -> List a -> List a)
=>  λ(Nil  : forall (a : *)                -> List a)
=>  λ(  foldr
    :   forall (a : *) -> List a -> forall (r : *) -> (a -> r -> r) -> r -> r
    )
=>  foldr Items (Cons Items i1 (Cons Items i2 (Nil Items)))
    	  VItems (λ(i : Items) => λ(_ : VItems) => vi1) vi2
)

-- List
(   λ(a : *)
=>  forall (list : *)
->  (a -> list -> list)  -- Cons
->  list                 -- Nil
->  list
)

-- Cons
(   λ(a : *)
=>  λ(va  : a)
=>  λ(vas : forall (list : *) -> (a -> list -> list) -> list -> list)
=>  λ(list : *)
=>  λ(Cons : a -> list -> list)
=>  λ(Nil  : list)
=>  Cons va (vas list Cons Nil)
)

-- Nil
(   λ(a : *)
=>  λ(list : *)
=>  λ(Cons : a -> list -> list)
=>  λ(Nil  : list)
=>  Nil
)

-- foldr
(   λ(a : *)
=>  λ(vas : forall (list : *) -> (a -> list -> list) -> list -> list)
=>  vas
)

)))))))