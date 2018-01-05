-- let Pair a b = P a b
--
-- in  result

( λ(result : *) ->

(   λ(DPair : forall (a : *) -> forall (P : (a -> *)) -> *)
 ->  λ(DP   : forall (a : *) -> forall (P : (a -> *)) -> forall (x: a) -> P x -> DPair a P)
 ->  λ(fst  : forall (a : *) -> forall (P : (a -> *)) -> forall (dp : DPair a P) -> a)
 ->  (λ(snd  : forall (a : *) -> forall (P : (a -> *)) -> forall (dp : DPair a P)
     	                      -> P (fst a P dp))
      ->  result)
       (   λ(a : *)
       ->  λ(P : (a -> *))
       ->  λ(dp : forall (r : *) -> (forall (x : a) -> P x -> r) -> r)
       ->  dp (P (fst a P dp)) (λ(x : a) -> λ(px : P (fst a P dp)) -> px)
       )
)

-- DPair (type constructor)
(λ(a : *) -> λ(P : (a -> *)) -> forall (r : *) -> (forall (x : a) -> P x -> r) -> r)

-- DP (value constructor)
(   λ(a : *)
->  λ(P : (a -> *))
->  λ(va : a)
->  λ(vP : P va)
->  λ(r : *)
->  λ(iDPair : forall (x : a) -> P x -> r)
->  iDPair va vP
)

-- fst
(   λ(a : *)
->  λ(P : (a -> *))
->  λ(dp : forall (r : *) -> (forall (x : a) -> P x -> r) -> r)
->  dp a (λ(x : a) -> λ(_ : P x) -> x)
)

-- TODO I don't know how to do this, but it turns out it may be possible
-- https://stackoverflow.com/questions/40773570/how-to-represent-arbitrary-gadts-on-morte
-- -- snd
-- (   λ(a : *)
-- ->  λ(P : (a -> *))
-- ->  λ(dp : forall (r : *) -> (forall (x : a) -> P x -> r) -> r)
-- ->  dp (P (
--  --begin copy of fst
--  (   λ(a : *)
--   ->  λ(P : (a -> *))
--   ->  λ(dp : forall (r : *) -> (forall (x : a) -> P x -> r) -> r)
--   ->  dp a (λ(x : a) -> λ(_ : P x) -> x)
--  )
--  --end copy of fst

--    a P dp)) (λ(x : a) -> λ(y : P x) -> y)
-- )

)

