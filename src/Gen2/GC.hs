{-# LANGUAGE QuasiQuotes #-}

module Gen2.GC where

import           Language.Javascript.JMacro

import           Data.Monoid

import           Gen2.RtsSettings
import           Gen2.RtsTypes
import           Gen2.Utils

haveWeakMap :: Bool
haveWeakMap = False

-- fixme collect static refs
-- fixme reset cafs
-- fixme scan weak refs
-- fixme unset stack stuff above sp
-- fixme unset unused registers
-- fixme alternative tagging without WeakMap
garbageCollector :: JStat
garbageCollector =
  [j| fun h$gc {
        var work = [];
        h$walkStack work `Stack` `Sp`;
        var reachable = new WeakMap();
        while(work.length > 0) {
          var c = work.pop();
          `followObject reachable work c`;
        }
      }
  
      fun h$walkStack work stack sp {
        for(var i=sp;i>=0;i--) {
          var c = stack[sp];
          if(`isHeapObject c`) {
            work.push(c);
          }
        }
      }
      `map mkFollowObject [(1::Int)..8]`;
    |]

-- | follow all data in the object, push
--   objects that are not reachable yet to work
-- fixme: probably needs some tweaking to make it fast with inline cache
followObject :: JExpr -> JExpr -> JExpr -> JStat
followObject reach work c =
  [j| var size = `c`.f.gtag & 0xFF;
      var d = `c`.d;
      `SwitchStat size (switchCases d) (switchDef d)`;
    |]
   where
     switchCases d = map (\n -> (toJExpr n, [j| `StrI ("h$followObject_" ++ show n)`(`d`) |])) [(1::Int)..8]
     switchDef d = [j| for(var i in `d`) {
                         if(`d`.hasOwnProperty(i)) {
                           var di = `d`[i];
                           `followField reach work di`;
                         }
                       }
                     |]

followField :: JExpr -> JExpr -> JExpr -> JStat
followField reach work d =
  [j| if(`isHeapObject d` && !`reach`.has(`d`)) {
        `work`.push(`d`);
      }
    |]

mkFollowObject n =
  [j| `decl funName`;
      `JVar funName` = `JFunc args body`;
    |]

   where
     reach   = StrI "reach"
     work    = StrI "work"
     c       = StrI "c"
     funName = StrI ("h$followObject_" ++ show n)
     args    = [reach,work,c]
     body    = [j| var x;
                   var d = `c`.d;
                   `map (collectField x d) [(1::Int)..n]`;
                 |]
     collectField x d n =
       [j| `x` = `SelExpr d (StrI ("d"++show n))`;
           `followField (toJExpr reach) (toJExpr work) x`;
         |]
-- | return true if it can be a heap object
isHeapObject :: JExpr -> JExpr
isHeapObject e = [je| typeof `e` === 'object'
                      && `e`.hasOwnProperty("f")
                      && `e`.hasOwnProperty("d")
                    |]
