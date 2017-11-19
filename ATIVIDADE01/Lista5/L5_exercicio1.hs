module Main where
 
import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, forM_)
import Data.List ((\\))
 
-- types
data Casa = Casa   
    { cor :: Cor      -- <trait> :: Casa -> <Trait>
    , homem   :: Homem
    , animal   :: Animal
    , bebida :: Bebida
    , fumo :: Fumo
    }
    deriving (Eq, Show)
 
data Cor = Vermelho | Verde | Azul | Amarelo | Branco
    deriving (Eq, Show, Enum, Bounded)
 
data Homem = Ingles | Sueco | Dane | Noruegues | Alemao
    deriving (Eq, Show, Enum, Bounded)
 
data Animal = Cachorro | Passaros | Gatos | Cavalo | Zebra
    deriving (Eq, Show, Enum, Bounded)
 
data Bebida = Cafe | Cha | Leite | Cerveja | Agua
    deriving (Eq, Show, Enum, Bounded)
 
data Fumo = PallMall | Dunhill | Blend | BlueMaster | Prince
    deriving (Eq, Show, Enum, Bounded)
 
type Solution = [Casa]
 
main :: IO ()
main = do
  forM_ solutions $ \sol -> mapM_ print sol
                            >> putStrLn "----"
  putStrLn "Nao ha mais solucoes!"
 
 
solutions :: [Solution]
solutions = filter finalCheck . map reverse $ foldM next [] [1..5]
    where
      -- NOTE: list of Casas is generated in reversed order
      next :: Solution -> Int -> [Solution]
      next sol pos = [h:sol | h <- newCasas sol, consistent h pos]
 
 
newCasas :: Solution -> Solution
newCasas sol =    -- all combinations of traits not yet used
    Casa <$> new cor <*> new homem <*> new animal <*> new bebida <*> new fumo
    where
      new trait = [minBound ..] \\ map trait sol  -- :: [<Trait>]
 
 
consistent :: Casa -> Int -> Bool
consistent casa pos = and                  -- consistent with the rules:
    [ homem   `is` Ingles     <=>   cor `is` Vermelho              --  2
    , homem   `is` Sueco     <=>   animal   `is` Cachorro              --  3
    , homem   `is` Dane     <=>   bebida `is` Cha              --  4
    , cor `is` Verde   <=>   bebida `is` Cafe           --  6
    , animal   `is` Passaros   <=>   fumo `is` PallMall         --  7
    , cor `is` Amarelo  <=>   fumo `is` Dunhill          --  8
    , const (pos == 3)   <=>   bebida `is` Leite             --  9
    , const (pos == 1)   <=>   homem   `is` Noruegues              -- 10
    , bebida `is` Cerveja    <=>   fumo `is` BlueMaster       -- 13
    , homem   `is` Alemao     <=>   fumo `is` Prince           -- 14
    ]
    where
      infix 4 <=>
      p <=> q  =  p casa == q casa   -- both True or both False
 
 
is :: Eq a => (Casa -> a) -> a -> Casa -> Bool
(trait `is` value) casa  =  trait casa == value
 
 
finalCheck :: [Casa] -> Bool
finalCheck solution = and                    -- fulfills the rules:
    [ (cor `is` Verde)   `leftOf` (cor `is` Branco)  --  5
    , (fumo `is` Blend  ) `nextTo` (animal   `is` Gatos )  -- 11
    , (fumo `is` Dunhill) `nextTo` (animal   `is` Cavalo)  -- 12
    , (cor `is` Azul   ) `nextTo` (homem   `is` Noruegues  )  -- 15
    , (fumo `is` Blend  ) `nextTo` (bebida `is` Agua)  -- 16
    ]
    where
      nextTo :: (Casa -> Bool) -> (Casa -> Bool) -> Bool
      nextTo p q = leftOf p q || leftOf q p
      leftOf p q 
          | (_:h:_) <- dropWhile (not . p) solution = q h
          | otherwise                               = False