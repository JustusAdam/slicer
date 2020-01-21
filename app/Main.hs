{-# LANGUAGE LambdaCase #-}

import Slicer
import Control.Lens hiding (indices)
import Data.Function
import Text.Printf
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type V = String

data Exp
    = I Int
    | BOp String Exp Exp
    | V V

flatten :: Exp -> [V]
flatten (V v) = [v]
flatten (BOp _ e1 e2) = [e1, e2] >>= flatten
flatten _ = []

instance Show Exp where
    show (I i) = show i
    show (BOp op o1 o2) = printf "%s %v %s" (show o1) op (show o2)
    show (V v) = v

data Op
    = Mult
    | Geq
    | Plus

data Stmt
    = Read [V]
    | Assign V Exp
    | If Exp
    | While Exp
    | Write [V]
    | Begin
    | End

instance Show Stmt where
    show = \case
        Read v -> "READ(" ++ intercalate "," v ++ ")"
        Write v -> "WRITE(" ++ intercalate "," v ++ ")"
        Assign v e -> v ++ " := " ++ show e
        If e -> "IF " ++ show e
        While e -> "WHILE " ++ show e
        Begin -> "BEGIN"
        End -> "END"

data Prog = Prog
    { prog :: [Stmt]
    , suc_ :: Int -> [Int]
    , infls :: Int -> S.Set Int
    }

intersections = \case
    [] -> S.empty
    (x:xs) -> foldl S.intersection x xs

mkinfls p = fromMaybe S.empty . flip M.lookup m
  where
    m = M.fromList $
      [ (n,) $ if length (suc p n) <= 1 then S.empty else
            let dom = minimum $ S.toList $ intersections $ map S.fromList $ paths n
            in S.fromList [n+1..dom-1]
      | n <- indices p
      ]
    paths n =
        case suc p n of
            [] -> [[]]
            sucs ->
                [n0:p0 | n0 <- sucs
                      , p0 <- paths n0]

testprog = Prog
    { prog =
      [ Begin
      , Read ["X", "Y"]
      , Assign "TOTAL" (I 0)
      , Assign "SUM" (I 0)
      , If (BOp "<=" (V "X") (I 1))
      , Assign "SUM" (V "Y")
      , Begin
      , Read ["Z"]
      , Assign "TOTAL" (BOp "*" (V "X") (V "Y"))
      , End
      , Write ["TOTAL", "SUM"]
      , End
      ]
    , infls = let dispatch = mkinfls testprog in \case
            6 -> S.fromList [7,8]
            9 -> S.fromList [7,8]
            i -> dispatch i
    , suc_ = \case
            4 -> [5, 6]
            5 -> [10]
            11 -> []
            i | i > 11 -> error $ "largest stmt is 11, got " ++ show i
              | otherwise -> [i+1]
    }

prog2 = Prog
    { prog =
      [ Assign "Y" (V "X")
      , Assign "A" (V "B")
      , Assign "Z" (V "Y")
      ]
    , infls = const S.empty
    , suc_ = \case
            2 -> []
            i | i > 2 -> error "Out of bounds"
              | otherwise -> [i+1]
    }

instance AST Prog where
    type Node Prog = Int
    type Var Prog = V
    indices Prog {prog} = [0..length prog - 1]
    suc Prog {suc_} = suc_
    ref Prog {prog} n = S.fromList $
        case prog !! n of
            Write v -> v
            If e -> flatten e
            While e -> flatten e
            Assign _ e -> flatten e
            _ -> []
    def Prog {prog} n =
        case prog !! n of
            Assign v _ -> S.singleton v
            Read v -> S.fromList v
            _ -> S.empty
    infl Prog {infls} = infls

-- data TStack
--     = Block
--     | Frame Int [Int]
--     deriving (Show)

-- sucs :: [Stmt] -> Int -> [Int]
-- sucs prog = fromMaybe mempty . flip M.lookup m
--   where
--     m = M.fromList $ fst $ foldl go ([], [])
--     go acc (me:s) = _4 %~ succ $ f acc
--       where
--         f = case s of
--           While _ -> addStack (me, Frame 1 [])
--           If _ -> addStack (me, Frame 2 [])
--           Begin -> addStack (me, Block)
--           End -> \acc -> case _2 acc of
--                            Block i:s -> handleStack i . setStack s
--                            i -> error $ "Closing frame " ++ show i
--           _ -> handleStack
--         s_top = head $ acc ^. _2
--         addStack t = _2 %~ (t:)
--         emit t = _1 %~ (t:)
--         setStack = ( _2 .~ )
--         handleStack begin ((fid,x):xs) = case x of
--             Block _ -> emit (i, [me+1])
--             Frame i l sucs
--                 | pred l == sucs -> let (entry, exit) = unzip $ (begin, me): sucs
--                   in _1 %~ ((fid, entry) : map (,[ me+1 ]) exit ++)
--                   emit (i, me:sucs) . handleStack xs
--                 | otherwise -> setStack $ (fid, Frame i l ((begin, me):sucs) ) : xs

printResult testprog criterion = forM_ (indices testprog) $ \stmt ->
    if inSlice stmt
    then printf "%s\n" (show $ prog testprog !! stmt)
    else pure ()
  where
    inSlice = slice testprog criterion

main = printResult testprog (11, S.singleton "Z")
--main = printResult testprog (11, S.singleton "TOTAL")
--main = printResult prog2 (3, S.singleton "Y")
