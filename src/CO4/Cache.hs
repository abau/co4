module CO4.Cache
  (Cache, numHits, numMisses, emptyCache, retrieve, cache)
where

import qualified Data.Map.Strict as M

data Cache k v = Cache { cacheMap  :: ! (M.Map k v)
                       , numHits   :: ! Int
                       , numMisses :: ! Int
                       }

emptyCache :: Cache k v
emptyCache = Cache M.empty 0 0

retrieve :: (Ord k) => k -> Cache k v -> (Maybe v, Cache k v)
retrieve key c = case M.lookup key $ cacheMap c of
  Nothing -> (Nothing, c { numMisses = 1 + numMisses c })
  Just v  -> (Just v , c { numHits   = 1 + numHits   c })

cache :: (Ord k) => k -> v -> Cache k v -> Cache k v
cache key value c = c { cacheMap = M.insert key value $ cacheMap c }
