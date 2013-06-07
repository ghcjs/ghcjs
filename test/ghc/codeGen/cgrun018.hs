{-# LANGUAGE MagicHash #-}
-- !!! test of datatype with many unboxed fields
--
import GHC.Base( Double# )
import GHC.Float

main = print (selectee1 + selectee2)

data Tfo = Tfo Double# Double# Double# Double# Double# Double# Double# Double# Double# Double# Double# Double#

yyy = (Tfo (-0.0018##)  (-0.8207##)   (0.5714##)
            (0.2679##)  (-0.5509##)  (-0.7904##)
            (0.9634##)   (0.1517##)   (0.2209##)
            (0.0073##)   (8.4030##)   (0.6232##))

xxx = (Tfo (-0.8143##)  (-0.5091##)  (-0.2788##)
           (-0.0433##)  (-0.4257##)   (0.9038##)
           (-0.5788##)   (0.7480##)   (0.3246##)
            (1.5227##)   (6.9114##)  (-7.0765##))

selectee1 = D# (case xxx of
                  Tfo _ _ _ _ _ _ _ x _ _ _ _ -> x)

selectee2 = D# (case xxx of
                  Tfo _ _ y _ _ _ _ _ _ _ _ _ -> y)
