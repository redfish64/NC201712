-- id2.mt

(   \(id : forall (a : *) -> forall(x:a) -> a)
->  id (forall (a : *) -> forall(x:a) -> a) id  -- Apply the identity function to itself
)

-- id
(\(a : *) -> \(x : a) -> x)