---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Samples.Modules where

import qualified Flowbox.System.UniPath              as UniPath
import qualified Flowbox.Batch.Project.Project       as Project
import           Flowbox.Batch.Project.Project         (Project)
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition(..))
import qualified Flowbox.Luna.Network.Graph.Graph    as Graph
import qualified Flowbox.Luna.Lib.LibManager         as LibManager
import           Flowbox.Luna.Lib.LibManager           (LibManager)
import qualified Flowbox.Luna.Lib.Library            as Library
import           Flowbox.Luna.Lib.Library              (Library(..))
import           Flowbox.Luna.Type.Type                (Type(..))


mkDefinition :: Type -> Definition
mkDefinition acls = Definition.empty{ Definition.cls = acls
                                    , Definition.graph = Graph.empty
                        	        }

mkModule :: String -> Definition
mkModule aname = mkDefinition (Module aname) 


mkClass :: String -> Definition
mkClass aname  = mkDefinition (Class aname [] [])


listToDefs :: [String] -> Definition.ID -> Definition.ID -> [(Definition.ID, Definition.ID, Definition)]
listToDefs l start parentID = map (\(i, n)-> (parentID, i, mkClass n)) $ zip [start..] l


wladcyPolski :: [String]
wladcyPolski = ["Bronislaw Komorowski", "Donald Tusk", "Lech Kaczynski", "Jaroslaw Kaczynski", "Leszek Miller", "Jerzy Buzek", "Wlodzimierz Cimoszewicz", "Jozef Oleksy", "Waldemar Pawlak", "Jan Krzysztof Bielecki", "Hanna Suchocka", "Aleksander Kwasniewski", "Lech Walesa", "Tadeusz Mazowiecki", "Wojciech Jaruzelski", "Mieczyslaw F. Rakowski", "Zbigniew Messner", "Henryk Jablonski", "Stanislaw Kania", "Edward Gierek", "Wladyslaw Gomulka", "Edward Ochab", "Jozef Cyrankiewicz", "Piotr Jaroszewicz", "Boleslaw Bierut", "Ignacy Moscicki", "Jozef Pilsudski", "Stanislaw Wojciechowski", "Franciszek Jozef", "Wanda", "Piast", "Siemomysl", "Kazimierz Wielki", "prof. Andrzej M. Skulimowski", "Wladyslaw Lokietek", "Mieszko I", "Krak", "Popiel", "Fryderyk August"]


atrybuty :: [String]
atrybuty = ["Berlo", "Konstytucja 3 maja", "iPad", "jablko", "korona", "front jednosci narodu", "lista wyborcza", "cenzura", "wstazka", "dwie wstazki", "order pracy ze wstazka", "order pracy bez wstazki", "partia", "narod", "komitet wyborczy", "wyborcy", "lud", "elity", "reputacja", "media", "stronnicze media", "obiektywne media", "praworzadnosc", "promienny usmiech", "usmiech do zlej gry", "kabel", "BOR", "garnitur", "mundur"]


addSomeDefs :: DefManager -> DefManager
addSomeDefs adefs = DefManager.addToParentMany (listToDefs atrybuty 2000 20)
                 $ DefManager.addToParent (0, 20, mkModule "atrybuty")
                 $ DefManager.addToParentMany (listToDefs wladcyPolski 1000 10)
                 $ DefManager.addToParent (0, 10, mkModule "wladcyPolski")
                 $ adefs

emptyStdLibrary :: Library
emptyStdLibrary = Library.make "std"           $ UniPath.fromUnixString "dummylibs/stdlib.lunalib"
    
     
userLibrary :: Library
userLibrary = Library.make "__workspace__" $ UniPath.fromUnixString "dummylibs/workspace.lunalib"

stdLibrary :: Library
stdLibrary  = emptyStdLibrary{Library.defs = addSomeDefs $ Library.defs emptyStdLibrary}        


alibManager :: LibManager
alibManager = LibManager.insNode (1, userLibrary)
            $ LibManager.insNode (0, stdLibrary)
            $ LibManager.empty


project :: Project
project = Project.empty { Project.name = "wladczy projekt"
                        , Project.path = UniPath.fromUnixString "sample-projects/wladcy" 
                        , Project.libs = alibManager 
                    	}
