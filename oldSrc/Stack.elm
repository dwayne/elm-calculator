module Stack exposing (Stack, new, push, pop)


type Stack a
  = Stack (List a)


new : Stack a
new =
  Stack []


push : a -> Stack a -> Stack a
push x (Stack xs) =
  Stack (x :: xs)


pop : Stack a -> Maybe (a, Stack a)
pop (Stack xs) =
  case xs of
    [] ->
      Nothing

    (x :: rest) ->
      Just (x, Stack rest)
