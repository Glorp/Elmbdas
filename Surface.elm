module Surface where
import open Term

bnd : (a -> Maybe b) -> Maybe a -> Maybe b
bnd f m = case m of
            Just x  -> f x
            Nothing -> Nothing

fmap : (a -> b) -> Maybe a -> Maybe b
fmap f m = bnd (\x -> Just (f x)) m

lam = case String.uncons "λ" of Just (c, _) -> c

seperator c = c == lam || c == '.' || c == '(' || c == ')' || white c

white c = c == ' ' || c == '\t' || c == '\n'

trim s = case String.uncons s of
           Just (c, rest) -> (if | white c -> trim rest
                                 | otherwise -> s)
           Nothing        -> ""


termstr x = case x of
              Lam s t                 -> concat ["λ", s, ".", termstr t, ""]
              App (Lam p b) a         -> concat [pstring (Lam p b), " ", argstring a]
              App f a                 -> concat [termstr f, " ", argstring a]
              Var s                   -> s

argstring a = case a of
               Var s -> s
               _     -> pstring a

pstring t = concat ["(", termstr t, ")"]


read : String -> Maybe Term
read s = bnd listToApp (readList (trim s))

listToApp : [Term] -> Maybe Term
listToApp l =
    let lToA x xs = case xs of
                      []      -> x
                      y :: ys -> lToA (App x y) ys
    in case l of
         []      -> Nothing
         x :: xs -> Just (lToA x xs)

readList s =
    let readL (c, s) = if | c == lam    -> fmap (\t -> [t]) (readLam s)
                          | c == '('    -> bnd (\(t, s) -> fmap (\ts -> t :: ts) (readList s))
                                           (readParen s)
                          | seperator c -> Nothing
                          | otherwise   -> (case readWord0 c (String.cons c s) of
                                              (w, rest) -> fmap (\ts -> (Var w) :: ts) (readList rest))
    in case String.uncons (trim s) of
               Just x  -> readL x
               Nothing -> Just [] 

readLam s = bnd (\(p, rest) -> fmap (\b -> Lam p b)
                                    (read rest))
                (readParam s)

readParam : String -> Maybe (String, String)
readParam s = bnd (\(p, s) -> (case String.uncons s of
                                      Just ('.', rest) -> Just (p, rest)
                                      _                -> Nothing))
                  (readWord s)

readParen str =
    let count s n l = bnd (\(c, s) -> if | c == ')' && l == 0 -> Just n
                                         | c == ')'           -> count s (n + 1) (l - 1)
                                         | c == '('           -> count s (n + 1) (l + 1)
                                         | otherwise          -> count s (n + 1) l)
                          (String.uncons s)
        res = fmap (\n -> (String.left n str, String.dropLeft (n + 1) str))
                   (count str 0 0)
    in bnd (\(s, rest) -> fmap (\t -> (t, rest)) (read s)) res

readWord : String -> Maybe (String, String)
readWord s = let trims = trim s
             in case String.uncons trims of
                  Just (c, rest) -> (if | seperator c -> Nothing
                                        | otherwise   -> Just (readWord0 c trims))
                  Nothing        -> Nothing

readWord0 c s = let readW c s n = if | seperator c -> (n - 1)
                                     | otherwise   -> (case String.uncons s of
                                                         Just (next, rest) -> readW next rest (n + 1)
                                                         Nothing           -> n)
                    n = readW c s 0
                in (String.left n s, String.dropLeft n s)
