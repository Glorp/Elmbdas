module Eval where

import Set

import open Term

data Step = L | F | A

findPath pred t = case findPath0 pred t [] of
                    Just x  -> Just (reverse x)
                    Nothing -> Nothing

findPathApp pred f a res = case findPath0 pred f (F :: res) of
                             Just x  -> Just x
                             Nothing -> findPath0 pred a (A ::res)

findPath0 pred t res = case pred t of
                        True  -> Just res
                        False -> case t of
                                   (Lam _ t) -> findPath0 pred t (L :: res)
                                   (App f a) -> findPathApp pred f a res
                                   _         -> Nothing

pathStep t b = case (t, b) of
                 (Lam _ t, L) -> t
                 (App f _, F) -> f
                 (App _ a, A) -> a

followPath t p = case p of
                   []       -> t
                   hd :: tl -> followPath (pathStep t hd) tl

followPathThen t p fn = case (t, p) of
                        (t, []) -> fn t
                        (Lam s t, L :: tl)    -> Lam s (followPathThen t tl fn)
                        (App f a, F :: tl)  -> App (followPathThen f tl fn) a
                        (App f a, A :: tl) -> App f (followPathThen a tl fn)

pathFirst t p pred current = case p of
                               []       -> Nothing
                               hd :: tl -> (if | pred t    -> Just (reverse current)
                                               | otherwise -> pathFirst (pathStep t hd) tl pred (hd :: current))

reducible t = case t of
                (App (Lam _ _) _) -> True
                _                 -> False

subst t s x = case x of
                App f a -> App (subst t s f) (subst t s a)
                Lam p b -> (if | p == s    -> Lam p b
                               | otherwise -> Lam p (subst t s b))
                Var v   -> (if | v == s    -> t
                               | otherwise -> Var v)

freeIds t env = case t of
                  Var s   -> (if | Set.member s env -> Set.empty
                                 | otherwise    -> Set.singleton s)
                  App f a -> Set.union (freeIds f env) (freeIds a env)
                  Lam p b -> freeIds b (Set.insert p env)

reduceRename t = case findPath reducible t of
                   Nothing -> Nothing
                   Just p  -> Just (followPathThen t p (reduceRename0 t))
                                

reduceRename0 t (App (Lam p b) a) = case conflict (Lam p b) a of
                                      Nothing -> subst a p b
                                      Just cp -> (case (findScope b cp (freeIds a Set.empty)) of
                                                    Just scp -> (App (Lam p (followPathThen b scp (rename t))) a))
rename t (Lam p b) = let s = uniqueId t p
                     in Lam s (subst (Var s) p b)

allIds t = case t of
             Lam p b -> Set.union (Set.singleton p) (allIds b)
             App f a -> Set.union (allIds f) (allIds a)
             Var s   -> Set.singleton s

uniqueId t s =
    let ids = (allIds t)
        uniq n = let newId = concat [s, show n]
                 in if | Set.member newId ids -> uniq (n + 1)
                          | otherwise         -> newId
    in uniq 2

findScope t p ids = pathFirst t
                              p
                              (\x -> case x of
                                       Lam p _ -> (if | Set.member p ids -> True
                                                    | otherwise -> False)
                                       _       -> False)
                              []

conflict (Lam param body) arg =
    let ids = (freeIds arg Set.empty)
        conf t poss res = case t of
                              Lam p b -> (if | p ==  param  -> Nothing
                                             | otherwise    -> conf b (poss || Set.member p ids) (L :: res))
                              App f a  -> (case (conf f poss (F :: res)) of
                                             Just x  -> Just x
                                             Nothing -> conf a poss (A :: res))
                              Var s    -> (if | s == param && poss  -> Just (reverse res)
                                              | otherwise         -> Nothing)
    in conf body False []
