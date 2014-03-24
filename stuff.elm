import String
import Graphics.Element as Element
import Graphics.Input as Input
import Graphics.Input.Field as Field
import Keyboard as Keys
import Term (Term, Lam, App, Var, Define, Undefine)
import Surface
import Eval

entered = keepIf id False Keys.enter

reduceN n mt = case mt of
                 Just t  -> (if | n == 0    -> [t]
                                | otherwise -> t :: reduceN (n - 1) (Eval.reduceRename t))
                 Nothing -> []

makeField s = let content = Input.input Field.noContent
                  fi = lift (Field.field Field.defaultStyle content.handle id s) content.signal
              in (fi, content.signal)

(field, content) = makeField "λ"

addRemoveDef trm lst = let add s t l =
                               case l of
                                 Define s2 t2 :: ts -> (if | s == s2   -> Define s t :: ts
                                                           | otherwise -> Define s2 t2 :: add s t ts)
                                 []                       -> [Define s t]
                           remove s l =
                               case l of
                                Define s2 t2 :: ts  -> (if | s == s2   -> ts
                                                           | otherwise -> Define s2 t2 :: remove s ts)
                                []                  -> []

                 in case trm of
                      Just (Define s t) -> add s t lst
                      Just (Undefine s) -> remove s lst
                      _                 -> lst


termsig = lift (Surface.readTerm . .string) content

defsig = foldp addRemoveDef [] (sampleOn entered termsig)

defs = lift (flow down) (lift (map (plainText . Surface.termstr)) defsig)

defTerm t = case t of
              Define _ _ -> True
              Undefine _ -> True
              _          -> False

tstr x defs = case x of
                Just t  -> if | defTerm t -> [Surface.termstr t]
                              | otherwise -> map Surface.termstr (reduceN 50 (Just (Eval.renameDefs t defs)))
                Nothing -> [":("]

mtermstr x = case x of
               Just t  -> map Surface.termstr (reduceN 100 (Just t))
               Nothing -> [":("]

link = (constant [markdown| [ploink](https://github.com/Glorp/Elmbdas) |])

main = let f = lift (width 1000) field
           x = lift (flow down) (lift (\x -> map plainText (mtermstr x)) termsig)
           a = lift (flow down) (lift2 (\x y -> map plainText (tstr x y)) termsig defsig)
       in lift (flow down) (combine [link, defs, f, a])

