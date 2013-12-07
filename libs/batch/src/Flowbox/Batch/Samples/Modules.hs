---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Samples.Modules where

--import           Flowbox.Prelude
--import qualified Flowbox.Batch.Project.Project       as Project
--import           Flowbox.Batch.Project.Project         (Project)
--import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
--import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
--import qualified Flowbox.Luna.Network.Def.Definition as Definition
--import           Flowbox.Luna.Network.Def.Definition   (Definition)
--import qualified Flowbox.Luna.Network.Graph.Graph    as Graph
--import qualified Flowbox.Luna.Lib.LibManager         as LibManager
--import           Flowbox.Luna.Lib.LibManager           (LibManager)
--import qualified Flowbox.Luna.Lib.Library            as Library
--import           Flowbox.Luna.Lib.Library              (Library)
--import           Flowbox.Luna.XOLD.Type.Type           (Type)
--import qualified Flowbox.Luna.XOLD.Type.Type         as Type
--import qualified Flowbox.Luna.Network.Attributes     as Attributes
--import qualified Flowbox.Luna.Network.Graph.Node     as Node
--import           Flowbox.Luna.Network.Graph.Node       (Node)
--import           Flowbox.Luna.Network.Graph.Value      (Value(Value))
--import           Flowbox.Luna.Network.Graph.Edge       (Edge(Edge))
--import qualified Flowbox.Luna.Network.Graph.Port     as Port
--import qualified Flowbox.System.UniPath              as UniPath
--import           Flowbox.System.UniPath                (UniPath)



--mkDefinition :: Type -> Definition
--mkDefinition acls = Definition.empty{ Definition.cls = acls
--                                    , Definition.graph = Graph.empty
--                                  }

--mkModule :: String -> Definition
--mkModule name = mkDefinition (Type.mkModule name )


--mkClass :: String -> Definition
--mkClass name  = mkDefinition (Type.mkClass name)


--mkFunction :: String -> Definition
--mkFunction name = mkDefinition (Type.Function name ( Type.Tuple [ Type.Tuple [ Type.Class "a" [] [Type.Class "g" [] []]
--                                                                , Type.Class "b" [] []
--                                                                , Type.Named "n" $ Type.Class "f" [] []
--                                                                ]
--                                                   , Type.Class "c" [] []
--                                                   , Type.Function "d" ( Type.Tuple [ Type.Class "e" [] []
--                                                                                    ]
--                                                                       )
--                                                                       ( Type.Tuple []
--                                                                       )
--                                                                ]
--                                                   )
--                                                   ( Type.Tuple [ Type.Class "o1" [] []
--                                                                , Type.Class "o2" [] []
--                                                                , Type.Class "o3" [] []
--                                                                ]
--                                                   )
--                               )


--listToDefs :: [String] -> Definition.ID -> Definition.ID -> (String -> Definition)
--           -> [(Definition.ID, Definition.ID, Definition)]
--listToDefs l start parentID mk = map (\(i, n)-> (parentID, i, mk n)) $ zip [start..] l



--wladcyPolski :: [String]
--wladcyPolski = ["BronislawKomorowski", "DonaldTusk", "LechKaczynski", "JaroslawKaczynski", "LeszekMiller", "JerzyBuzek", "WlodzimierzCimoszewicz", "JozefOleksy", "WaldemarPawlak", "JanKrzysztofBielecki", "HannaSuchocka", "AleksanderKwasniewski", "LechWalesa", "TadeuszMazowiecki", "WojciechJaruzelski", "MieczyslawFRakowski", "ZbigniewMessner", "HenrykJablonski", "StanislawKania", "EdwardGierek", "WladyslawGomulka", "EdwardOchab", "JozefCyrankiewicz", "PiotrJaroszewicz", "BoleslawBierut", "IgnacyMoscicki", "JozefPilsudski", "StanislawWojciechowski", "FranciszekJozef", "Wanda", "Piast", "Siemomysl", "KazimierzWielki", "ProfAndrzejMSkulimowski", "WladyslawLokietek", "MieszkoI", "Krak", "Popiel", "FryderykAugust"]


--atrybuty :: [String]
--atrybuty = ["berlo", "konstytucja3maja", "iPad", "jablko", "korona", "frontJednosciNarodu", "listaWyborcza", "cenzura", "wstazka", "dwieWstazki", "orderPracyZeWstazka", "orderPracyBezWstazki", "partia", "narod", "komitetWyborczy", "wyborcy", "lud", "elity", "reputacja", "media", "stronniczeMedia", "obiektywneMedia", "praworzadnosc", "promiennyUsmiech", "usmiechDoZlejGry", "kabel", "bOR", "garnitur", "mundur"]


--cls_console :: Definition
--cls_console = Definition.empty { Definition.cls   = Type.Class "Console" [] []
--                               , Definition.graph = Graph.empty
--                               }


--cls_vector :: Definition
--cls_vector = Definition.empty{ Definition.cls   = Type.Class "Vector" ["a"] [Type.Named "x" (Type.TypeName "a"), Type.Named "y" (Type.TypeName "a"), Type.Named "z" (Type.TypeName "a")]
--                             , Definition.graph = Graph.empty
--                             }


--addSomeDefs :: DefManager -> DefManager
--addSomeDefs defs = DefManager.addToParentMany (listToDefs atrybuty     2000 20 mkFunction)
--                 $ DefManager.addToParent (0, 20, mkModule "atrybuty")
--                 $ DefManager.addToParentMany (listToDefs wladcyPolski 1000 10 mkClass)
--                 $ DefManager.addToParent (0, 10, mkModule "wladcyPolski")
--                 $ DefManager.addToParent (4, 5 , func_vec_incx)
--                 $ DefManager.addToParent (3, 4 , cls_vector)
--                 $ DefManager.addToParent (1, 3 , mkModule "Math")
--                 $ DefManager.addToParent (1, 2 , mkModule "IO")
--                 $ DefManager.addToParent (0, 1 , mkModule "Std")
--                 $ defs


--func_vec_incx_graph :: LibManager.Gr Node Edge
--func_vec_incx_graph = Graph.insEdges [
--                              (0, 1, Edge Port.All Port.All),
--                              (1, 2, Edge Port.All Port.All),
--                              (2, 3, Edge Port.All Port.All),
--                              (3, 4, Edge Port.All Port.All),
--                              (5, 6, Edge Port.All Port.All),
--                              (4, 6, Edge Port.All Port.All),
--                              (6, 7, Edge Port.All Port.All),
--                              (7, 8, Edge Port.All Port.All),
--                              (8, 9, Edge Port.All Port.All),
--                              (1, 9, Edge Port.All Port.All),
--                              (9, 10, Edge Port.All Port.All),
--                              (10, 11, Edge Port.All Port.All)
--                             ]

--           $ Graph.insNodes [(0,  Node.mkInputs             ),
--                             (1,  Node.mkExpr     "select0" ),
--                             (2,  Node.mkNTuple             ),
--                             (3,  Node.mkExpr     "x'getter"),
--                             (4,  Node.mkExpr     "select0" ),
--                             (5,  Node.Default (Value "1") Attributes.empty),
--                             (6,  Node.mkNTuple             ),
--                             (7,  Node.mkExpr     "add"     ),
--                             (8,  Node.mkExpr     "select0" ),
--                             (9,  Node.mkNTuple             ),
--                             (10, Node.mkExpr     "x'setter"),
--                             (11, Node.mkOutputs            )
--                            ]
--           $ Graph.empty


--func_vec_incx_inputs :: Type
--func_vec_incx_inputs = Type.Tuple [Type.Named "self" $ Type.TypeName "a",
--                                   Type.Named "in2"  $ Type.TypeName "a"]


--func_vec_incx :: Definition
--func_vec_incx = Definition.empty{ Definition.cls   = (Type.Function "incx" func_vec_incx_inputs Type.noOutputs)
--                      , Definition.graph = func_vec_incx_graph
--                      }


--emptyStdLibrary :: UniPath -> Library
--emptyStdLibrary rootpath = Library.make "std" $ UniPath.append "stdlib.lunalib" rootpath


--userLibrary :: UniPath -> Library
--userLibrary rootpath = Library.make "workspace" $ UniPath.append "workspace.lunalib" rootpath


--stdLibrary :: UniPath -> Library
--stdLibrary rootpath = lib{Library.defs = addSomeDefs $ Library.defs lib} where
--    lib = emptyStdLibrary rootpath


--libManager :: UniPath -> LibManager
--libManager rootpath = LibManager.insNode (1, userLibrary rootpath)
--                    $ LibManager.insNode (0, stdLibrary  rootpath)
--                    $ LibManager.empty


--project :: Project
--project = addDefaultLibraries
--        $ Project.empty { Project.name = "wladczy projekt"
--                        , Project.path = UniPath.fromUnixString "sample-projects/wladcy"
--                        , Project.libs = LibManager.empty
--                        } where


--addDefaultLibraries :: Project -> Project
--addDefaultLibraries proj = proj {Project.libs = libManager rootpath} where
--    rootpath = Project.path proj
