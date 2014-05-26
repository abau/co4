{-# OPTIONS_CO4 SizedNum Nat3 #-}

data Bool = False | True;

data Num = Zero | Bit0 Num | Bit1 Num;

main x = -- equals (succ x) (Bit1 (Bit1 Zero))
    equals (plus x x) (Bit0 (Bit0 (Bit1 Zero)))

-- succ :: Num -> Num;
succ x =
  (case x of {
    Zero -> Bit1 Zero;
    Bit0 a -> Bit1 a;
    Bit1 xa -> Bit0 (succ xa);
  });

-- plus :: Num -> Num -> Num;
plus x y =
  (case x of {
    Zero -> y;
    Bit0 xa ->
      (case y of {
        Zero -> xa;
        Bit0 ya -> Bit0 (plus xa ya);
        Bit1 ya -> Bit1 (plus xa ya);
      });
    Bit1 xa ->
      (case y of {
        Zero -> xa;
        Bit0 ya -> Bit1 (plus xa ya);
        Bit1 ya -> Bit0 (succ (plus xa ya));
      });
  });

-- equals :: Num -> Num -> Bool;
equals x y =
  (case x of {
    Zero ->
      (case y of {
        Zero -> True;
        Bit0 a -> equals x a;
        Bit1 _ -> False;
      });
    Bit0 xa ->
      (case y of {
        Zero -> equals xa y;
        Bit0 a -> equals xa a;
        Bit1 _ -> False;
      });
    Bit1 xa ->
      (case y of {
        Zero -> False;
        Bit0 _ -> False;
        Bit1 a -> equals xa a;
      });
  });


