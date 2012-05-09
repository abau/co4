simple_constraint bs =  
  let bla = True 
  in
    head ( map (\b -> b || bla) bs )

map f xs = case xs of [] -> [] ; x : xs -> f x : map f xs          
head (x:xs) = x
