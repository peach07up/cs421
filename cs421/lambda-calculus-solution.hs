data LExp = Abs String LExp
          | App LExp LExp
          | Var String
  deriving (Show,Eq)

numOccurs :: LExp -> String -> Int
numOccurs (Abs x le) y
  | x == y = 1 + numOccurs le y
  | otherwise = numOccurs le y
numOccurs (App l1 l2) y = (numOccurs l1 y) + (numOccurs l2 y)
numOccurs (Var x) y
  | x == y = 1
  | otherwise = 0

numOccurs' :: LExp -> LExp -> Int
numOccurs' le (Var x) = numOccurs le x
numOccurs' _ _ = error "Invalid state"

distinctVars :: LExp -> Int
distinctVars le = length (varList le)
  where varList :: LExp -> [String]
        varList (Var x) = [x]
        varList (Abs x le)
          | elem x (varList le) = (varList le)
          | otherwise = x : (varList le)
        varList (App l1 l2) = (varList l1) ++ [ x | x <- (varList l2), not (elem x (varList l1))]
