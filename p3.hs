{-# LANGUAGE GADTs #-}

-- Imports for Monads

import Control.Monad

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving (Show,Eq)

type Env = [(String,FAE)]

evalDynFAE :: Env -> FAE -> (Maybe FAE)
evalDynFAE _ (Num x) = (Just (Num x))
evalDynFAE env (Plus l r) = do
    l' <- (evalDynFAE env l)
    r' <- (evalDynFAE env r)
    case l' of
        (Num n1) -> case r' of
            (Num n2) -> Just (Num (n1 + n2))
            _ -> (Nothing)
        _ -> (Nothing)
evalDynFAE env (Minus l r) = do
    l' <- (evalDynFAE env l)
    r' <- (evalDynFAE env r)
    case l' of
        (Num n1) -> case r' of
            (Num n2) -> Just (Num (n1 - n2))
            _ -> (Nothing)
        _ -> (Nothing)
evalDynFAE _ (Lambda i b) = (Just (Lambda i b))
evalDynFAE env (App f a) = do
    (Lambda i b) <- (evalDynFAE env f)
    a' <- (evalDynFAE env a)
    evalDynFAE ((i,a'):env) b
evalDynFAE env (Id id) = do
    v <- (lookup id env)
    Just v

data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving (Show,Eq)
  
type Env' = [(String,FAEValue)]

evalStatFAE :: Env' -> FAE -> (Maybe FAEValue)
evalStatFAE _ _ = Nothing
evalStatFAE _ (Num x) = (Just (NumV x))
evalStatFAE env (Plus l r) = do
    l' <- (evalStatFAE env l)
    r' <- (evalStatFAE env r)
    case l' of
        (NumV n1) -> case r' of
            (NumV n2) -> Just (NumV (n1 + n2))
            _ -> (Nothing)
        _ -> (Nothing)
evalStatFAE env (Minus l r) = do
    l' <- (evalStatFAE env l)
    r' <- (evalStatFAE env r)
    case l' of
        (NumV n1) -> case r' of
            (NumV n2) -> Just (NumV (n1 - n2))
            _ -> (Nothing)
        _ -> (Nothing)
evalStatFAE env (Lambda i b) = (Just (ClosureV i b env))
evalStatFAE env (App f a) = do
    (ClosureV i b e) <- (evalStatFAE env f)
    a' <- (evalStatFAE env a)
    evalStatFAE ((i,a'):env) b
evalStatFAE env (Id id) = do
    v <- (lookup id env)
    Just v

-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving (Show,Eq)

elabFBAE :: FBAE -> FAE
elabFBAE _ = (Num (-1))
elabFBAE (NumD x) = (Num x)
elabFBAE (PlusD l r) = (Plus (elabFBAE l)(elabFBAE r))
elabFBAE (MinusD l r) = (Minus (elabFBAE l)(elabFBAE r))
elabFBAE (LambdaD i b) = (Lambda i (elabFBAE b))
elabFBAE (AppD f a) = (App (elabFBAE f)(elabFBAE a))
elabFBAE (BindD i v b) = (App (Lambda i (elabFBAE b)) (elabFBAE v))
elabFBAE (IdD id) = (Id id)

evalFBAE :: Env' -> FBAE -> (Maybe FAEValue)
evalFBAE env t = (evalStatFAE env (elabFBAE t))

-- FBAEC AST and Type Definitions

data FBAEC where
  NumE :: Int -> FBAEC
  PlusE :: FBAEC -> FBAEC -> FBAEC
  MinusE :: FBAEC -> FBAEC -> FBAEC
  TrueE :: FBAEC
  FalseE :: FBAEC
  AndE :: FBAEC -> FBAEC -> FBAEC
  OrE :: FBAEC -> FBAEC -> FBAEC
  NotE :: FBAEC -> FBAEC
  IfE :: FBAEC -> FBAEC -> FBAEC -> FBAEC
  LambdaE :: String -> FBAEC -> FBAEC
  AppE :: FBAEC -> FBAEC -> FBAEC
  BindE :: String -> FBAEC -> FBAEC -> FBAEC
  IdE :: String -> FBAEC
  deriving (Show,Eq)

elabFBAEC :: FBAEC -> FAE
elabFBAEC _ = (Num (-1))

evalFBAEC :: Env' -> FBAEC -> Maybe FAEValue
evalFBAEC _ _ = Nothing
