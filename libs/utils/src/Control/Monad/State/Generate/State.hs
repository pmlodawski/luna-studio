
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE CPP                       #-}

module Control.Monad.State.Generate.State where

#if __GLASGOW_HASKELL__ >= 710

import           Control.Applicative
import qualified Control.Monad.Trans.State as State
import           Language.Haskell.TH       hiding (appsE, classP)
import           Prelude

newState :: String -> Name -> Q [Dec]
newState name el = sequence [ genDataType
                            , genAlias
                            , genTypeClass
                            , genMonadSelfInst
                            , genMonadStateInst
                            , genTransInstance
                            , gen_runT
                            , gen_run
                            , gen_evalT
                            , gen_eval
                            , gen_with
                            ]

    where
        dataName  = mkName $ name
        transName = mkName $ name ++ "T"
        getName   = mkName "get"
        putName   = mkName "put"
        liftName  = mkName "lift"
        className = mkName $ "Monad" ++ name
        fieldName = mkName $ "un" ++ name ++ "T"

        n_applicative = mkName "Applicative"
        n_monad       = mkName "Monad"
        n_functor     = mkName "Functor"
        n_monadState  = mkName "State.MonadState"
        n_monadTrans  = mkName "MonadTrans"
        n_runStateT   = mkName "State.runStateT"
        n_evalStateT  = mkName "State.evalStateT"
        n_runIdentity = mkName "runIdentity"
        n_Identity    = mkName "Identity"
        n_return      = mkName "return"

        a         = mkName "a"
        b         = mkName "b"
        elCon     = ConT el

        -- newtype MyDataT m a = MyDataT { unMyDataT :: StateT Value m a }
        --                       deriving (Monad, MonadIO, MonadPlus, Applicative, Alternative, Functor)
        genDataType :: Q Dec
        genDataType = do
            m <- newName "m"
            t <- newName "t"
            let stateT    = mkName "State.StateT"
                derivings = fmap mkName [ "Functor"
                                        , "Monad"
                                        , "MonadIO"
                                        , "MonadPlus"
                                        , "MonadTrans"
                                        , "Applicative"
                                        , "Alternative"
                                        ]
            ctx <- mapM conT derivings
            return $ NewtypeD [] transName
                        [PlainTV m,PlainTV t]
                        Nothing
                        (RecC transName [(fieldName, Bang NoSourceUnpackedness NoSourceStrictness, appsT (ConT stateT) [elCon, VarT m, VarT t])])
                        ctx

        -- type MyData = MyDataT Identity
        genAlias :: Q Dec
        genAlias = return $ TySynD dataName [] (AppT (ConT transName) (ConT n_Identity))

        genTypeClass :: Q Dec
        genTypeClass = do
            m <- newName "m"
            let getFunc  = SigD getName (AppT (VarT m) elCon)
                putFunc  = SigD putName (AppT (AppT ArrowT elCon) (AppT (VarT m) (TupleT 0)))
                predics  = fmap (flip classP [VarT m]) [n_monad, n_applicative]
            return $ ClassD predics className [PlainTV m] [] [getFunc, putFunc]


        --instance (Monad m, Functor m) => MonadMyData (MyDataT m) where
        --    get = MyDataT $ State.get
        --    put = MyDataT . State.put
        genMonadSelfInst :: Q Dec
        genMonadSelfInst = do
            m <- VarT <$> newName "m"
            let stateGet = mkName "State.get"
                statePut = mkName "State.put"
            return $ InstanceD Nothing [classP n_monad [m], classP n_functor [m]]
                         (appsT (ConT className) [AppT (ConT transName) m])
                         [ mkFunc getName []       $ appChainE (ConE transName) [VarE stateGet]
                         , mkFunc putName [VarP a] $ appChainE (ConE transName) [VarE statePut, VarE a]
                         ]


        --instance MonadState s m => MonadState s (MyDataT m) where
        --    get = MyDataT . lift $ State.get
        --    put = MyDataT . lift . State.put
        genMonadStateInst :: Q Dec
        genMonadStateInst = do
            s <- VarT <$> newName "s"
            m <- VarT <$> newName "m"
            let stateGet = mkName "State.get"
                statePut = mkName "State.put"
            return $ InstanceD Nothing [classP n_monadState [s, m]]
                         (appsT (ConT n_monadState) [s, AppT (ConT transName) m])
                         [ mkFunc getName []       $ appChainE (ConE transName) [VarE liftName, VarE stateGet]
                         , mkFunc putName [VarP a] $ appChainE (ConE transName) [VarE liftName, VarE statePut, VarE a]
                         ]


        --instance (MonadMyData m, MonadTrans t, Monad (t m), Applicative (t m)) => MonadMyData (t m) where
        --    get = lift get
        --    put = lift . put
        genTransInstance :: Q Dec
        genTransInstance = do
            s <- VarT <$> newName "s"
            m <- VarT <$> newName "m"
            let t = VarT $ mkName "t"
                premise = [ classP className     [m]
                          , classP n_monadTrans  [t]
                          , classP n_monad       [AppT t m]
                          , classP n_applicative [AppT t m]
                          ]
            return $ InstanceD Nothing premise
                         (appsT (ConT className) [AppT t m])
                         [ mkFunc getName []       $ appChainE (VarE liftName) [VarE getName]
                         , mkFunc putName [VarP a] $ appChainE (VarE liftName) [VarE putName, VarE a]
                         ]

        --runT :: MyDataT m a -> Value -> m (a, Value)
        --runT = runStateT . unPragmaStoreT
        n_runT = mkName "runT"
        gen_runT :: Q Dec
        gen_runT = return $ mkFunc n_runT [VarP a] $ appChainE (VarE n_runStateT) [VarE fieldName, VarE a]

        --run :: PragmaStore a -> Value -> (a,Value)
        --run = runIdentity .: runT
        n_run = mkName "run"
        gen_run :: Q Dec
        gen_run = return $ mkFunc n_run [VarP a, VarP b] $ appChainE (VarE n_runIdentity) [appsE (VarE n_runT) [VarE a, VarE b]]

        --evalT :: Monad m => MyDataT m a -> Value -> m a
        --evalT = evalStateT . unPragmaStoreT
        n_evalT = mkName "evalT"
        gen_evalT :: Q Dec
        gen_evalT = return $ mkFunc n_evalT [VarP a] $ appChainE (VarE n_evalStateT) [VarE fieldName, VarE a]

        --eval :: MyData a -> Value -> a
        --eval = runIdentity .: evalT
        n_eval = mkName "eval"
        gen_eval :: Q Dec
        gen_eval = return $ mkFunc n_eval [VarP a, VarP b] $ appChainE (VarE n_runIdentity) [appsE (VarE n_evalT) [VarE a, VarE b]]

        --with :: MonadMyData m => (Val -> Val) -> m a -> m a
        --with f m = do
        --    a <- get
        --    put (f a)
        --    out <- m
        --    put a
        --    return out
        n_with = mkName "with"
        gen_with :: Q Dec
        gen_with = return $ mkFunc n_with [VarP f,VarP m]
                 $ DoE [ BindS (VarP a) (VarE getName)
                       , NoBindS (AppE (VarE putName) (AppE (VarE f) (VarE a)))
                       , BindS (VarP out) (VarE m)
                       , NoBindS (AppE (VarE putName) (VarE a))
                       , NoBindS (AppE (VarE n_return) (VarE out))
                       ]
                  where out = mkName "out"
                        f   = mkName "f"
                        m   = mkName "m"

mkFunc name args func = FunD name [Clause args (NormalB func) []]

appsE b apps = foldl AppE b apps
appsT b apps = foldl AppT b apps

appChainE b as = appChainE' (b:as)
    where appChainE' = \case
              [a]    -> a
              (a:as) -> AppE a $ appChainE' as


classP base = foldl AppT (ConT base)

#endif
