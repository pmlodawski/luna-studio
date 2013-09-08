---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Samples.Modules where

import qualified Flowbox.Batch.Project.Project           as Project
import           Flowbox.Batch.Project.Project             (Project)
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Definition       (Definition(..))
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import qualified Flowbox.Luna.Lib.LibManager             as LibManager
import           Flowbox.Luna.Lib.LibManager               (LibManager)
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Luna.Lib.Library                  (Library(..))
import           Flowbox.Luna.XOLD.Type.Type               (Type(..))
import qualified Flowbox.Luna.XOLD.Type.Type             as Type
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node)
import qualified Flowbox.Luna.Network.Graph.DefaultValue as DefaultValue
import qualified Flowbox.Luna.Network.Graph.Edge         as Edge
import           Flowbox.Luna.Network.Graph.Edge           (Edge(..))
import qualified Flowbox.System.UniPath                  as UniPath
import           Flowbox.System.UniPath                    (UniPath)



mkDefinition :: Type -> Definition
mkDefinition acls = Definition.empty{ Definition.cls = acls
                                    , Definition.graph = Graph.empty
                                  }

mkModule :: String -> Definition
mkModule aname = mkDefinition (Module aname) 


mkClass :: String -> Definition
mkClass aname  = mkDefinition (Class aname [] [])


mkFunction :: String -> Definition
mkFunction aname = mkDefinition (Function aname ( Tuple [ Tuple [ Class "a" [] [Class "g" [] []]
                                                                , Class "b" [] []
                                                                , List $ Named "n" $ Class "f" [] []
                                                                ]
                                                        , Class "c" [] []
                                                        , Function "d" ( Tuple [ Class "e" [] []
                                                                               ]
                                                                       ) 
                                                                       ( Tuple []
                                                                       )
                                                        ]
                                                 ) 
                                                 ( Tuple [ Class "o1" [] []
                                                         , Class "o2" [] []
                                                         , Class "o3" [] []
                                                         ]
                                                 )
                                 )


listToDefs :: [String] -> Definition.ID -> Definition.ID -> (String -> Definition)
           -> [(Definition.ID, Definition.ID, Definition)]
listToDefs l start parentID mk = map (\(i, n)-> (parentID, i, mk n)) $ zip [start..] l



wladcyPolski :: [String]
wladcyPolski = ["Bronislaw Komorowski", "Donald Tusk", "Lech Kaczynski", "Jaroslaw Kaczynski", "Leszek Miller", "Jerzy Buzek", "Wlodzimierz Cimoszewicz", "Jozef Oleksy", "Waldemar Pawlak", "Jan Krzysztof Bielecki", "Hanna Suchocka", "Aleksander Kwasniewski", "Lech Walesa", "Tadeusz Mazowiecki", "Wojciech Jaruzelski", "Mieczyslaw F. Rakowski", "Zbigniew Messner", "Henryk Jablonski", "Stanislaw Kania", "Edward Gierek", "Wladyslaw Gomulka", "Edward Ochab", "Jozef Cyrankiewicz", "Piotr Jaroszewicz", "Boleslaw Bierut", "Ignacy Moscicki", "Jozef Pilsudski", "Stanislaw Wojciechowski", "Franciszek Jozef", "Wanda", "Piast", "Siemomysl", "Kazimierz Wielki", "prof. Andrzej M. Skulimowski", "Wladyslaw Lokietek", "Mieszko I", "Krak", "Popiel", "Fryderyk August"]


atrybuty :: [String]
atrybuty = ["Berlo", "Konstytucja 3 maja", "iPad", "jablko", "korona", "front jednosci narodu", "lista wyborcza", "cenzura", "wstazka", "dwie wstazki", "order pracy ze wstazka", "order pracy bez wstazki", "partia", "narod", "komitet wyborczy", "wyborcy", "lud", "elity", "reputacja", "media", "stronnicze media", "obiektywne media", "praworzadnosc", "promienny usmiech", "usmiech do zlej gry", "kabel", "BOR", "garnitur", "mundur"]


cls_console :: Definition
cls_console = Definition.empty { Definition.cls   = Type.Class "Console" [] []
                               , Definition.graph = Graph.empty
                               }


cls_vector :: Definition
cls_vector = Definition.empty{ Definition.cls   = Type.Class "Vector" ["a"] [Type.Named "x" (Type.TypeVariable "a"), Type.Named "y" (Type.TypeVariable "a"), Type.Named "z" (Type.TypeVariable "a")]
                    , Definition.graph = Graph.empty
                    }


addSomeDefs :: DefManager -> DefManager
addSomeDefs adefs = DefManager.addToParentMany (listToDefs atrybuty     2000 20 mkClass)
                  $ DefManager.addToParent (0, 20, mkModule "atrybuty")
                  $ DefManager.addToParentMany (listToDefs wladcyPolski 1000 10 mkFunction)
                  $ DefManager.addToParent (0, 10, mkModule "wladcyPolski")
                  $ DefManager.addToParent (4, 5 , func_vec_incx)
                  $ DefManager.addToParent (3, 4 , cls_vector)
                  $ DefManager.addToParent (1, 3 , mkModule "Math")
                  $ DefManager.addToParent (1, 2 , mkModule "IO")
                  $ DefManager.addToParent (0, 1 , mkModule "Std")
                  $ adefs


func_vec_incx_graph :: LibManager.Gr Node Edge
func_vec_incx_graph = Graph.insEdges [
                              (0, 1, Edge.standard),
                              (1, 2, Edge.standard),
                              (2, 3, Edge.standard),
                              (3, 4, Edge.standard),
                              (5, 6, Edge.standard),
                              (4, 6, Edge.standard),
                              (6, 7, Edge.standard),
                              (7, 8, Edge.standard),
                              (8, 9, Edge.standard),
                              (1, 9, Edge.standard),
                              (9, 10, Edge.standard),
                              (10, 11, Edge.standard)
                             ]

           $ Graph.insNodes [(0,  Node.mkInputs             ),
                             (1,  Node.mkExpr     "select0" ),
                             (2,  Node.mkNTuple             ),
                             (3,  Node.mkExpr     "x'getter"),
                             (4,  Node.mkExpr     "select0" ),
                             (5,  Node.Default (DefaultValue.DefaultInt "1") Attributes.empty),
                             (6,  Node.mkNTuple             ),
                             (7,  Node.mkExpr     "add"     ),
                             (8,  Node.mkExpr     "select0" ),
                             (9,  Node.mkNTuple             ),
                             (10, Node.mkExpr     "x'setter"),
                             (11, Node.mkOutputs            )
                            ]
           $ Graph.empty


func_vec_incx_inputs :: Type
func_vec_incx_inputs = Type.Tuple [Type.Named "self" $ Type.TypeVariable "a", 
                                   Type.Named "in2" $ Type.TypeVariable "a"]


func_vec_incx :: Definition
func_vec_incx = Definition.empty{ Definition.cls   = (Type.Function "incx" func_vec_incx_inputs Type.noOutputs)
                      , Definition.graph = func_vec_incx_graph
                      }


emptyStdLibrary :: UniPath -> Library
emptyStdLibrary rootpath = Library.make "std" $ UniPath.append "stdlib.lunalib" rootpath
    
     
userLibrary :: UniPath -> Library
userLibrary rootpath = Library.make "workspace" $ UniPath.append "workspace.lunalib" rootpath


stdLibrary :: UniPath -> Library
stdLibrary rootpath = lib{Library.defs = addSomeDefs $ Library.defs lib} where
    lib = emptyStdLibrary rootpath


libManager :: UniPath -> LibManager
libManager rootpath = LibManager.insNode (1, userLibrary rootpath)
                    $ LibManager.insNode (0, stdLibrary  rootpath)
                    $ LibManager.empty


project :: Project
project = addDefaultLibraries 
        $ Project.empty { Project.name = "wladczy projekt"
                        , Project.path = UniPath.fromUnixString "sample-projects/wladcy"
                        , Project.libs = LibManager.empty
                        } where


addDefaultLibraries :: Project -> Project
addDefaultLibraries proj = proj {Project.libs = libManager rootpath} where
    rootpath = Project.path proj
