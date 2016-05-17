module ForSyDeDSL where

data Expr = LIT Integer
          | ADD Expr Expr
          | MUL Expr Expr
          | SUB Expr Expr  
          | DIV Expr Expr  
          | NEG Expr
          | ABS Expr
          | SIGNUM Expr
          | IFELSE Expr Expr Expr
          deriving (Show, Eq)

instance Num Expr where
   fromInteger n = LIT n
   e1 + e2       = ADD e1 e2
   e1 - e2       = SUB e1 e2
   e1 * e2       = MUL e1 e2
   abs e         = ABS e
   signum e      = SIGNUM e

data Signal = NullS
            | CONS Expr Signal

     
{-
data Signal a = Sig [a]
              | MapSY (a -> a) (Signal a) 
              | ZipWithSY (a -> a -> a) (Signal a) (Signal a)  
              | DelaySY a (Signal a)
--              | Compose (MapSY (a -> a)) (MapSY (a -> a)) (Signal a) 
-}
                
eval :: Expr -> Integer
eval (LIT x) = x
eval (ADD a b) = eval a + eval b
eval (MUL a b) = eval a * eval b
eval (SUB a b) = eval a - eval b
eval (DIV a b) = eval a - eval b
eval (NEG x)   = -eval (x)
eval (ABS x)   = abs (eval x)
eval (SIGNUM x) = signum (eval x)
eval (IFELSE p x y) = if (eval p == 1) then (eval x) else (eval y)     



{-
sum (LIT 0) = 0
sum n       = ADD n (sum (SUB n (LIT 1)))
-}

{-
fac (LIT 0) = LIT 1
fac (LIT 1) = LIT 1
fac (LIT n) = MUL (LIT n) (fac (SUB (LIT n) (LIT 1)))
fac (SUB (LIT 1) (LIT 1)) = LIT 1
fac (SUB n (LIT 1)) = MUL (SUB (LIT n) (LIT 1)) (fac (SUB (SUB (LIT n) (LIT 1)) (LIT 1)))     
-}

-- More patterns
-- fac n = MUL n (fac (SUB n (LIT 1)))


            
{-     
distribution_law :: Expr -> Expr
distribution_law (ADD (MUL a b) (MUL c d))
  | a == c    = MUL a (ADD b d)  
  | a == d    = MUL a (ADD b c)           
  | b == c    = MUL b (ADD a d)  
  | b == d    = MUL b (ADD a c)                         
  | otherwise = ADD (MUL a b) (MUL c d)           
distribution_law exp = exp   

simplify_exp ::Expr -> Expr
simplify_exp exp = distribution_law exp

eval_exp :: Expr -> Integer
eval_exp expr = eval $ simplify_exp expr

term1 = ADD (MUL (LIT 1) (LIT 2)) (MUL (LIT 3) (LIT 4)) 
term2 = ADD (MUL (LIT 1) (LIT 2)) (MUL (LIT 3) (LIT 1)) 

run :: Signal Token -> Signal Integer
run (Sig xs) = Sig (map eval xs)
run (MapSY f (Sig xs)) = Sig (map (eval . f) xs)
run (ZipWithSY f (Sig xs) (Sig ys)) = Sig (map eval $ zipWith f xs ys)
run (DelaySY v (Sig xs)) = Sig (map eval (v:xs))

-- run1 (Compose (MapSY f) (MapSY g) (Sig xs)) = 0

--run (ZIPWITHSY f) (Sig xs) (Sig ys) = Sig (zipWith f xs)
--run (DELAYSY f) init (Sig xs) = Sig (init:xs)

add1 = ADD (LIT 1)

--system = Compose (MapSY add1) (MapSY add1) (Sig [LIT 1, LIT 2])

sig2List (Sig xs) = xs

synth :: Expr -> String
synth (LIT x) = show x
synth (ADD a b) = synth a ++ "+" ++ synth b
synth (MUL a b) = synth a ++ "*" ++ synth b
synth (SUB a b) = synth a ++ "-" ++ synth b
synth (DIV a b) = synth a ++ "/" ++ synth b
synth (NEG x)   = "-" ++ synth (x) 
-}
