-- ( \ ( a : *) -> \ ( b : *) ->
--     a b)

-- ( λ(result : *) ->
--   (
--    \ (type : *) -> (\ (f : type) -> result)
--   )
--   (forall (r : *) -> r -> r)
--   (\ (t : *) -> \ (x : t) -> x)
-- )

( λ(result : *) ->
  (
   \ (type : *) ->
     (\ (f : type) -> result)
       (\ (t : *) -> \ (x : t) -> x)
  )
  (forall (r : *) -> r -> r)
)


-- ( λ(result : *) ->
-- ( λ(FavThings : *) ->
-- ( λ(tree : FavThings) ->
-- ( λ(fish : FavThings) ->

--   (
--   \ ( a : *) ->
--   \ ( b : *) ->
--   \ (x : (forall (f : * -> *) -> *) ) ->
--   result
--   )
--   (\ (v1 : FavThings) -> tree)
-- ))))
    