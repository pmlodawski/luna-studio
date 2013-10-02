---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Samples.Std where

import           Flowbox.Prelude                           
import qualified Flowbox.Batch.Project.Project           as Project
import           Flowbox.Batch.Project.Project             (Project)
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Definition       (Definition)
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import qualified Flowbox.Luna.Lib.LibManager             as LibManager
import           Flowbox.Luna.Lib.LibManager               (LibManager)
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Luna.Lib.Library                  (Library)
import           Flowbox.Luna.XOLD.Type.Type               (Type)
import qualified Flowbox.Luna.XOLD.Type.Type             as Type
import qualified Flowbox.System.UniPath                  as UniPath
import           Flowbox.System.UniPath                    (UniPath)



mkDefinition :: Type -> Definition
mkDefinition acls = Definition.empty { Definition.cls = acls
                                     , Definition.graph = Graph.empty
                                     }

mkModule :: String -> Definition
mkModule name = mkDefinition (Type.mkModule name ) 


mkClass :: String -> Definition
mkClass name  = mkDefinition (Type.mkClass name)


mkFunction :: String -> Definition
mkFunction name = mkDefinition (Type.Function name ( Type.Tuple [ Type.Tuple [ Type.Class "a" [] [Type.Class "g" [] []]
                                                                , Type.Class "b" [] []
                                                                , Type.Named "n" $ Type.Class "f" [] []
                                                                ]
                                                   , Type.Class "c" [] []
                                                   , Type.Function "d" ( Type.Tuple [ Type.Class "e" [] []
                                                                                    ]
                                                                       ) 
                                                                       ( Type.Tuple []
                                                                       )
                                                                ]
                                                   ) 
                                                   ( Type.Tuple [ Type.Class "o1" [] []
                                                                , Type.Class "o2" [] []
                                                                , Type.Class "o3" [] []
                                                                ]
                                                   )
                               )


listToDefs :: [String] -> Definition.ID -> Definition.ID -> (String -> Definition)
           -> [(Definition.ID, Definition.ID, Definition)]
listToDefs l start parentID mk = map (\(i, n)-> (parentID, i, mk n)) $ zip [start..] l



wladcyPolski :: [String]
wladcyPolski = ["BronislawKomorowski", "DonaldTusk", "LechKaczynski", "JaroslawKaczynski", "LeszekMiller", "JerzyBuzek", "WlodzimierzCimoszewicz", "JozefOleksy", "WaldemarPawlak", "JanKrzysztofBielecki", "HannaSuchocka", "AleksanderKwasniewski", "LechWalesa", "TadeuszMazowiecki", "WojciechJaruzelski", "MieczyslawFRakowski", "ZbigniewMessner", "HenrykJablonski", "StanislawKania", "EdwardGierek", "WladyslawGomulka", "EdwardOchab", "JozefCyrankiewicz", "PiotrJaroszewicz", "BoleslawBierut", "IgnacyMoscicki", "JozefPilsudski", "StanislawWojciechowski", "FranciszekJozef", "Wanda", "Piast", "Siemomysl", "KazimierzWielki", "ProfAndrzejMSkulimowski", "WladyslawLokietek", "MieszkoI", "Krak", "Popiel", "FryderykAugust"]


atrybuty :: [String]
atrybuty = ["berlo", "konstytucja3maja", "iPad", "jablko", "korona", "frontJednosciNarodu", "listaWyborcza", "cenzura", "wstazka", "dwieWstazki", "orderPracyZeWstazka", "orderPracyBezWstazki", "partia", "narod", "komitetWyborczy", "wyborcy", "lud", "elity", "reputacja", "media", "stronniczeMedia", "obiektywneMedia", "praworzadnosc", "promiennyUsmiech", "usmiechDoZlejGry", "kabel", "bOR", "garnitur", "mundur"]


types :: [String]
types = ["Int", "Float", "String", "Char"]


cls_console :: Definition
cls_console = Definition.empty { Definition.cls   = Type.Class "Console" [] []
                               , Definition.graph = Graph.empty
                               }


addSomeDefs :: DefManager -> DefManager
addSomeDefs defs = DefManager.addToParentMany (listToDefs types 1000 2 mkClass)
                 $ DefManager.addToParent (0, 2 , mkModule "Types")
                 $ DefManager.addToParent (0, 1 , mkModule "Math")
                 $ defs


func_vec_incx_inputs :: Type
func_vec_incx_inputs = Type.Tuple [Type.Named "self" $ Type.TypeName "a", 
                                   Type.Named "in2"  $ Type.TypeName "a"]


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
