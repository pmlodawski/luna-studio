{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}

module Empire.ASTOps.Parse (
    SomeParserException
  , FunctionParsing(..)
  , parseExpr
  , parsePattern
  , parsePortDefault
  , runParser
  , runFunHackParser
  -- , runReparser
  , runProperParser
  , runProperVarParser
  , runProperPatternParser
  ) where

import           Data.Convert
import           Data.IORef
import           Empire.Empire
import           Empire.Prelude hiding (mempty)
import           Prologue (convert, convertVia, mempty, wrap)

import           Control.Monad.Catch          (catchAll)
import           Data.Char                    (digitToInt)
import qualified Data.Text                    as Text

import           Empire.ASTOp                    (EmpirePass, GraphOp)
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.Graph               (ClsGraph, Graph)
import qualified Empire.Data.Graph               as Graph (codeMarkers)
import           Empire.Data.Layers              (attachEmpireLayers, SpanLength)
-- import           Empire.Data.Parser              (ParserPass)
import qualified Empire.Commands.Code            as Code
import qualified Control.Monad.State.Layered as State

import           LunaStudio.Data.PortDefault     (PortDefault (..), PortValue (..))

import qualified Data.Text.Position              as Pos
import qualified Data.Vector.Storable.Foreign    as Vector
-- import qualified Luna.Builtin.Data.Function      as Function (compile)
import qualified Luna.IR.Layer as Layer
import qualified Luna.IR                         as IR
import qualified OCI.Pass.Definition.Declaration      as Pass
import qualified Luna.Pass                         as Pass
import qualified Luna.Pass.Scheduler as Scheduler
-- import qualified Luna.Runner as Runner
-- import qualified Luna.Syntax.Text.Layer.Loc      as Loc
import           Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.IR.Class as Token
import qualified Luna.Syntax.Text.Parser.Pass as Parser
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
import qualified Luna.Syntax.Text.Parser.Data.Name.Special as Parser (uminus)
import qualified Data.Graph.Data.Graph.Class as LunaGraph
import qualified Empire.Pass.PatternTransformation            as PT
import qualified Luna.Pass.Attr as Attr
-- import qualified Luna.Syntax.Text.Parser.Marker  as Parser (MarkedExprMap(..))
-- import qualified Luna.Syntax.Text.Parser.Parser  as Parser
import qualified Luna.Syntax.Prettyprint as Prettyprint
import qualified Luna.Syntax.Text.Parser.IR.Term as Parsing
import Luna.Syntax.Text.Scope (Scope)
import qualified Luna.Syntax.Text.Source         as Source
import qualified Luna.IR.Term.Literal            as Lit
import Luna.Syntax.Text.Parser.Pass.Class (IRBS, Parser)
import Luna.Syntax.Text.Parser.Data.Result (Result (Result))
-- import qualified OCI.Pass                        as Pass

data SomeParserException = forall e. Exception e => SomeParserException e

deriving instance Show SomeParserException

instance Exception SomeParserException where
    toException = astExceptionToException
    fromException = astExceptionFromException
    displayException exc = case exc of SomeParserException e -> "SomeParserException (" <> displayException e <> ")"

-- parseExpr :: GraphOp m => String -> m NodeRef
-- parseExpr s = do
--     putAttr @Source.Source $ convert s
--     Parsing.parsingPassM Parsing.expr
--     res     <- getAttr @Parser.ParsedExpr
--     exprMap <- getAttr @MarkedExprMap
--     return $ unwrap' res

-- parsePattern :: GraphOp m => Text.Text -> m NodeRef
-- parsePattern s = do
--     putAttr @Source.Source $ convert s
--     Parsing.parsingPassM Parsing.pattern
--     res     <- getAttr @Parser.ParsedExpr
--     exprMap <- getAttr @MarkedExprMap
--     return $ unwrap' res

-- parserBoilerplate :: PMStack IO ()
-- parserBoilerplate = do
--     IR.runRegs
--     Loc.init
--     IR.attachLayer 5 (getTypeDesc @Pos.Range)         (getTypeDesc @IR.AnyExpr)
--     CodeSpan.init
--     IR.attachLayer 5 (getTypeDesc @CodeSpan.CodeSpan) (getTypeDesc @IR.AnyExpr)
--     IR.setAttr (getTypeDesc @MarkedExprMap)   $ (mempty :: MarkedExprMap)
--     IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
--     IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
--     IR.setAttr (getTypeDesc @Invalids) $ (mempty :: Invalids)

-- type OnDemandPass pass = (Typeable pass, Pass.Compile pass IO)

-- runPass' :: forall pass. OnDemandPass pass => Pass.Pass pass () -> IO (Scheduler.State)
-- runPass' = runPasses . pure

-- newtype ParsedExpr = ParsedExpr (NodeRef, MarkedExprMap)
-- type instance Attr.Type ParsedExpr = Attr.Atomic
-- instance Default ParsedExpr where def = error "empty parsedexpr"

-- runPasses :: forall pass. OnDemandPass pass => [Pass.Pass pass ()] -> IO (Scheduler.State)
-- runPasses passes = Scheduler.runManual reg sched where
--     reg = do
--         Runner.registerAll
--         Parser.registerStatic
--     sched = do
--         Parser.registerDynamic
--         Scheduler.registerAttr @ParsedExpr
--         Scheduler.enableAttrByType @ParsedExpr
--         for_ passes $ \pass -> do
--             Scheduler.registerPassFromFunction__ pass -- ONLY FOR TEST SPEC
--             Scheduler.runPassByType @pass
--         State.get @Scheduler.State

-- shouldParseAs :: Token.Parser (IRBS IR.SomeTerm) -> Text -> Text
--               {- -> (Delta, Delta)-} -> IO ()
-- shouldParseAs parser input output {-desiredSpan-} = runPass' $ do
--     (((ir,cs),scope), _) <- flip Parser.runParser__ (convert input) $ do
--         irb   <- parser
--         scope <- State.get @Scope
--         let Parser.IRBS irx = irb
--             irb' = Parser.IRBS $ do
--                 ir <- irx
--                 cs <- Layer.read @CodeSpan ir
--                 pure (ir,cs)
--         pure $ (,scope) <$> irb'
    -- genCode <- Prettyprint.run @Prettyprint.Simple scope ir

    -- let span = convert $ view CodeSpan.realSpan cs :: (Delta,Delta)
    -- genCode `shouldBe` output
    -- span `shouldBe` desiredSpan

parseExpr s = view _1 <$> runParser Parsing.expr s `catchAll` (\e -> throwM $ SomeParserException e)
parsePattern s = view _1 <$> runParser Parsing.pat s `catchAll` (\e -> throwM $ SomeParserException e)

passConverter :: (stage1 ~ stage2) => Pass.Pass stage1 pass1 a -> Pass.Pass stage2 pass2 a
passConverter = unsafeCoerce


runParser :: Token.Parser (IRBS IR.SomeTerm) -> Text -> IO (NodeRef, LunaGraph.State PT.EmpireStage, Scheduler.State, MarkedExprMap)
runParser parser input = do
    -- r <- newIORef (error "empty runParser")
    -- st <- runPass' $ do
    --     (((ir,cs),scope), m) <- flip Parser.runParser__ (convert input) $ do
    --         irb   <- parser
    --         scope <- State.get @Scope
    --         let Parser.IRBS irx = irb
    --             irb' = Parser.IRBS $ do
    --                 ir <- irx
    --                 cs <- Layer.read @CodeSpan ir
    --                 pure (ir,cs)
    --         pure $ (,scope) <$> irb'
    --     marker <- IR.marker 666
    --     markedNode <- IR.marked marker ir
    --     matchExpr markedNode print
    --     liftIO $ writeIORef r (ir, m)
    --     genCode <- Prettyprint.run @Prettyprint.Simple scope ir
    --     print "generated:" >> print genCode
    --     return ()
    -- -- case ref of
    -- --     Just (Result ref') -> return (generalize ref', def)
    -- --     _                  -> error "runParser"
    -- (ref, cs) <- readIORef r
    -- return (generalize ref, st, cs)
    -- putStrLn "foo"
    (((ir, m), scState), grState) <- LunaGraph.encodeAndEval @PT.EmpireStage $ do
        foo <- Scheduler.runT $ do
            ref <- liftIO $ newIORef (error "emptyreturn")
            Scheduler.registerPassFromFunction__ @PT.EmpireStage @EmpirePass $ do
                ((ir,scope), m) <- passConverter $ flip Parser.runParser__ (convert input) $ do
                    irb   <- parser
                    scope <- State.get @Scope
                    let Parser.IRBS irx = irb
                        irb' = Parser.IRBS $ do
                            ir <- irx
                            -- cs <- Layer.read @CodeSpan ir
                            -- putStrLn "CodeSpan" >> print cs
                            pure ir
                    pure $ (,scope) <$> irb'
                liftIO $ writeIORef ref $ generalize (ir, m)
                -- putStrLn "bar"
                -- Attr.put $ PassReturnValue $ unsafeCoerce $ Just a
                -- b <- Attr.get @PassReturnValue
                -- b <- liftIO $ readIORef ref
                -- print a
                -- print ir
                -- matchExpr ir $ \case
                --     Unit _ _ c -> do
                --         c' <- source c
                --         matchExpr c' $ \case
                --             ClsASG _ _ _ _ funs'' -> do
                --                 funs <- ptrListToList funs''
                --                 spans <- mapM (Layer.read @CodeSpan <=< source) funs
                --                 print spans
                --     a -> print a
            Scheduler.runPassByType @EmpirePass
            -- st <- Layered.get @Scheduler.State
            -- let [a] = Map.elems (st ^. Scheduler.attrs)
            -- putStrLn "BOOM"
            -- let Just (PassReturnValue foo) = unsafeCoerce a
            -- putStrLn "BOOMBOX"
            -- let p = (unsafeCoerce foo :: IR.SomeTerm)
            -- print p
            -- ret <- Scheduler.lookupAttr @PassReturnValue
            -- case ret of
            --     Nothing -> error "runASTOp: pass didn't register return value"
            --     Just (PassReturnValue foo) -> case unsafeCoerce foo of
            --         Just a -> return a
            --         _      -> error "default: empty return"
            foo <- liftIO $ readIORef ref
            -- st <- Layered.get @Scheduler.State
            return foo
        st <- LunaGraph.getState
        return (foo, st)
    -- print "returnedparser"
    return (ir, grState, scState, m)

instance Convertible Text Source.Source where
    convert t = Source.Source (convertVia @String t)

-- runProperParser :: Text.Text -> IO (NodeRef, IR.Rooted NodeRef, MarkedExprMap)
runProperParser :: Text.Text -> IO (NodeRef, LunaGraph.State PT.EmpireStage, Scheduler.State, MarkedExprMap)
runProperParser code = runParser Parsing.unit' code `catchAll` (\e -> throwM $ SomeParserException e) -- do
    -- runPM $ do
    --     parserBoilerplate
    --     attachEmpireLayers
    --     IR.setAttr (getTypeDesc @Source.Source) $ (convert code :: Source.Source)
    --     (unit, root) <- Pass.eval' @ParserPass $ do
    --         Parsing.parsingPassM Parsing.unit' `catchAll` (\e -> throwM $ SomeParserException e)
    --         res  <- getAttr @Parser.ParsedExpr
    --         root <- Function.compile (unwrap' res)
    --         return (unwrap' res, root)
    --     Just exprMap <- unsafeCoerce <$> IR.unsafeGetAttr (getTypeDesc @MarkedExprMap)
    --     return (unit, root, exprMap)

runProperVarParser :: Text.Text -> IO ()
runProperVarParser code = (void $ runParser Parsing.var code) `catchAll` (\e -> throwM $ SomeParserException e)
    -- runPM $ do
    --     parserBoilerplate
    --     attachEmpireLayers
    --     IR.setAttr (getTypeDesc @Source.Source) $ (convert code :: Source.Source)
    --     var <- Pass.eval' @ParserPass $ do
    --         Parsing.parsingPassM Parsing.var `catchAll` (\e -> throwM $ SomeParserException e)
    --         res  <- getAttr @Parser.ParsedExpr
    --         return (unwrap' res)
    --     return var

runProperPatternParser :: Text.Text -> IO NodeRef
runProperPatternParser code = parsePattern code -- do
    -- runPM $ do
    --     parserBoilerplate
    --     attachEmpireLayers
    --     IR.setAttr (getTypeDesc @Source.Source) $ (convert code :: Source.Source)
    --     pattern <- Pass.eval' @ParserPass $ do
    --         Parsing.parsingPassM Parsing.pattern `catchAll` (\e -> throwM $ SomeParserException e)
    --         res  <- getAttr @Parser.ParsedExpr
    --         return (unwrap' res)
    --     return pattern

-- runParser :: Text.Text -> Command Graph (NodeRef, MarkedExprMap)
-- runParser expr = do
--     let inits = do
--             return ()
--             -- IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

--             -- IR.setAttr (getTypeDesc @MarkedExprMap)   $ (mempty :: MarkedExprMap)
--             -- IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
--             -- IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
--             -- IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
--         run = runPass @Graph @ParserPass inits
--     run $ do
--         Parsing.parsingPassM Parsing.expr `catchAll` (\e -> throwM $ SomeParserException e)
--         res     <- getAttr @Parser.ParsedExpr
--         exprMap <- getAttr @MarkedExprMap
--         return (unwrap' res, exprMap)

prepareInput :: Text.Text -> FunctionParsing -> Text.Text
prepareInput expr parsing = Text.concat $ header : case parsing of
    AppendNone -> [":\n    None"]
    ParseAsIs  -> []
    where
        stripped = Text.strip expr
        header   = case Text.splitOn " " stripped of
            (def:var:args) -> Text.intercalate " " (def:var:args)
            i              -> Text.concat i


data FunctionParsing = AppendNone | ParseAsIs

runFunHackParser :: Text.Text -> FunctionParsing -> Command ClsGraph (NodeRef, Text.Text)
runFunHackParser expr parsing = do
    let input = prepareInput expr parsing
    parse <- runFunParser input
    return (view _1 parse, input)

runFunParser :: Text.Text -> Command ClsGraph (NodeRef, LunaGraph.State PT.EmpireStage, Scheduler.State, MarkedExprMap)
runFunParser expr = liftIO $
    runParser (Parsing.possiblyDocumented Parsing.func) expr
        `catchAll` (\e -> throwM $ SomeParserException e)
    -- let inits = do
    --         return ()
            -- IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

            -- IR.setAttr (getTypeDesc @MarkedExprMap)   $ (mempty :: MarkedExprMap)
            -- IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
            -- IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
            -- IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
    --     run = runPass @ClsGraph @ParserPass inits
    -- run $ do
    --     Parsing.parsingPassM (Parsing.possiblyDocumented $ (Parsing.rootedRawFunc <|> Parsing.func)) `catchAll` (\e -> throwM $ SomeParserException e)
    --     res     <- getAttr @Parser.ParsedExpr
    --     exprMap <- getAttr @MarkedExprMap
    --     return (unwrap' res, exprMap)

-- runReparser :: Text.Text -> NodeRef -> Command Graph (NodeRef, MarkedExprMap, Parser.ReparsingStatus)
-- runReparser expr oldExpr = do
--     let inits = do
--             return ()
--             -- IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

--             -- IR.setAttr (getTypeDesc @MarkedExprMap)   $ (mempty :: MarkedExprMap)
--             -- IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
--             -- IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (wrap' oldExpr :: Parser.ParsedExpr)
--             -- IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
--         run = runPass @Graph @ParserPass inits
--     run $ do
--         do
--             gidMapOld <- use Graph.codeMarkers

--             -- parsing new file and updating updated analysis
--             Parsing.parsingPassM Parsing.valExpr `catchAll` (\e -> throwM $ SomeParserException e)
--             gidMap    <- getAttr @MarkedExprMap

--             -- Preparing reparsing status
--             rs        <- Parsing.cmpMarkedExprMaps (wrap' gidMapOld) gidMap
--             putAttr @Parser.ReparsingStatus (wrap rs)

--         res     <- getAttr @Parser.ParsedExpr
--         exprMap <- getAttr @MarkedExprMap
--         status  <- getAttr @Parser.ReparsingStatus
--         return (unwrap' res, exprMap, status)

data PortDefaultNotConstructibleException = PortDefaultNotConstructibleException PortDefault
    deriving Show

instance Exception PortDefaultNotConstructibleException where
    toException = astExceptionToException
    fromException = astExceptionFromException

infixr 0 `withLength`
withLength :: GraphOp m => m NodeRef -> Int -> m NodeRef
withLength act len = do
    ref <- act
    putLayer @SpanLength ref (convert len)
    return ref



parsePortDefault :: GraphOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)          = do
    ref <- liftIO $ parseExpr (convert expr)
    Code.propagateLengths ref
    return ref
parsePortDefault (Constant (IntValue  i))
    | i >= 0     = do
        intPart <- Vector.fromList $ map (fromIntegral . digitToInt) $ show i
        generalize <$> IR.number 10 intPart Vector.empty `withLength` (length $ show i)
    | otherwise = do
        intPart <- Vector.fromList $ map (fromIntegral . digitToInt) $ show (abs i)
        number <- generalize <$> IR.number 10 intPart Vector.empty `withLength` (length $ show $ abs i)
        minus  <- generalize <$> IR.var Parser.uminus `withLength` 1
        app    <- generalize <$> IR.app minus number `withLength` (1 + length (show (abs i)))
        return app
parsePortDefault (Constant (TextValue s)) = do
    l <- Vector.fromList s
    generalize <$> IR.rawString l `withLength` (length s)
parsePortDefault (Constant (RealValue d)) = do
    let (int, frac) = properFraction d
    intPart <- Vector.fromList $ map (fromIntegral . digitToInt) $ show int
    fracPart <- Vector.fromList $ map (fromIntegral . digitToInt) $ show frac
    generalize <$> IR.number 10 intPart fracPart `withLength` (length $ show d)
parsePortDefault (Constant (BoolValue b)) = generalize <$> IR.cons (convert $ show b) []  `withLength` (length $ show b)
parsePortDefault d = throwM $ PortDefaultNotConstructibleException d
