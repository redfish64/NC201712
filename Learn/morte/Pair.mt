( λ(result : *) ->
( λ(FavThings : *) ->
( λ(tree : FavThings) ->
( λ(fish : FavThings) ->


-- let Pair a b = P a b
--
-- in  result

(   λ(Pair : * -> * -> *)
->  λ(P    : forall (a : *) -> forall (b : *) -> a -> b -> Pair a b)
->  λ(fst  : forall (a : *) -> forall (b : *) -> Pair a b -> a)
->  λ(snd  : forall (a : *) -> forall (b : *) -> Pair a b -> b)
->
   --result
   snd FavThings FavThings (P FavThings FavThings tree fish)
)

-- Pair
(λ(a : *) -> λ(b : *) -> forall (r : *) -> (a -> b -> r) -> r)

-- P
(   λ(a : *)
->  λ(b : *)
->  λ(va : a)
->  λ(vb : b)
->  λ(r : *)
->  λ(Pairx : a -> b -> r)
->  Pairx va vb
)

-- fst
(   λ(a : *)
->  λ(b : *)
->  λ(p : forall (r : *) -> (a -> b -> r) -> r)
->  p a (λ(x : a) -> λ(_ : b) -> x)
)

-- snd
(   λ(a : *)
->  λ(b : *)
->  λ(p : forall (r : *) -> (a -> b -> r) -> r)
->  p b (λ(_ : a) -> λ(x : b) -> x)
)

))))