import String
import Graphics.Element as Element
import Graphics.Input as Input
import open Term
import open Surface
import open Eval

parse s = case (read s) of
            Just t  -> map termstr (reduceN 50 (Just t))
            Nothing -> [":("]

reduceN n mt = case mt of
                 Just t  -> (if | n == 0    -> [t]
                                | otherwise -> t :: reduceN (n - 1) (reduceRename t))
                 Nothing -> []

(field, content) = Input.field "λ"

link = [markdown| [ploink](https://github.com/Glorp/Elmbdas) |]
res = lift (flow down) (lift (\x -> map plainText (parse x)) content)

main = lift (above link) (lift2 above (lift (width 1000) field) res)
