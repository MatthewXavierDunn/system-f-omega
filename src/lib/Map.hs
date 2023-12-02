module Map where

import Data.List (find)

type Map k v = [(k, v)]

empty :: Map k v
empty = []

delete :: (Eq k) => k -> Map k v -> Map k v
delete k = filter ((/= k) . fst)

insert :: (Eq k) => k -> v -> Map k v -> Map k v
insert k v m = (k, v) : delete k m

get :: (Eq k) => k -> Map k v -> Maybe v
get k m = snd <$> find ((== k) . fst) m

extend :: (Eq k) => Map k v -> Map k v -> Map k v
extend [] m = m
extend ((k, v) : ms) m = extend ms (insert k v m)

has :: (Eq k) => k -> Map k v -> Bool
has k = any ((== k) . fst)
