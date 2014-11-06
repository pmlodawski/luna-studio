module Logger.Internal.Stack where

import Control.Monad          (join)



data Log e = CallResult String (Maybe String) [Log e]
           | Call       String Bool           [Log e]
           | Log        String
           | Error      String

instance Show e => Show (Log e) where
  showList = (++) . formatStack False
  show x = formatStack True [x]


formatStack :: (Show e) => Bool -> [Log e] -> String
formatStack fulldebug = concatMap (printStack 0)
  where printStack :: Int -> Log e -> String
        printStack n (CallResult str (Just a) stck2) = concat
          [ indent n
          , "CALL: "++str++" … -> "++show a++"\n"
          , if fulldebug then concatMap (printStack $ n+1) stck2 else ""
          ]
        printStack n (CallResult str Nothing stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , " …\n"
          , concatMap (printStack $ n+1) stck2
          ]
        printStack n (Call str True stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , "\n"
          , if fulldebug then concatMap (printStack $ n+1) stck2 else ""
          ]
        printStack n (Call str False stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , "\n"
          , concatMap (printStack $ n+1) stck2
          ]
        printStack n (Log str) = concat
          [ indent n
          , "LOG : "
          , str
          , "\n"
          ]
        printStack n (Error str) = concat
          [ indent n
          , "ERR : "
          , str
          , "\n"
          ]
        indent n = if n>0
                 then join (replicate (n-1) "    ") ++ "  - "
                 else ""