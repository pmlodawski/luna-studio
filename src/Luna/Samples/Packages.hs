---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Samples.Packages where


import qualified Luna.Project                    as Project
import           Luna.Project                      (Project(..))
import qualified Luna.Network.Def.DefManager     as DefManager
import           Luna.Network.Def.DefManager       (DefManager(..))
import qualified Luna.Network.Def.Definition     as Definition
import           Luna.Network.Def.Definition       (Definition(..))
import qualified Luna.Network.Graph.Graph        as Graph
import qualified Luna.Lib.LibManager             as LibManager
import           Luna.Lib.LibManager               (LibManager(..))
import qualified Luna.Lib.Library                as Library
import           Luna.Lib.Library                  (Library(..))
import qualified Luna.System.UniPath             as UniPath
import           Luna.System.UniPath               (UniPath)
import qualified Luna.Type.Type                  as Type
import           Luna.Type.Type                    (Type(..))


mkDefinition cls libID = Definition.empty{ Definition.cls = cls
                            , Definition.graph = Graph.empty
                            , Definition.libID = libID }

mkModule name = mkDefinition (Module name) 
mkClass name  = mkDefinition (Class name [] [])

listToDefs l start parentID libID= map (\(i, n)-> (parentID, i, mkClass n libID)) $ zip [start..] l

wladcyPolski = ["Bronislaw Komorowski", "Donald Tusk", "Lech Kaczynski", "Jaroslaw Kaczynski", "Leszek Miller", "Jerzy Buzek", "Wlodzimierz Cimoszewicz", "Jozef Oleksy", "Waldemar Pawlak", "Jan Krzysztof Bielecki", "Hanna Suchocka", "Aleksander Kwasniewski", "Lech Walesa", "Tadeusz Mazowiecki", "Wojciech Jaruzelski", "Mieczyslaw F. Rakowski", "Zbigniew Messner", "Henryk Jablonski", "Stanislaw Kania", "Edward Gierek", "Wladyslaw Gomulka", "Edward Ochab", "Jozef Cyrankiewicz", "Piotr Jaroszewicz", "Boleslaw Bierut", "Ignacy Moscicki", "Jozef Pilsudski", "Stanislaw Wojciechowski", "Franciszek Jozef", "Wanda", "Piast", "Siemomysl", "Kazimierz Wielki", "prof. Andrzej M. Skulimowski", "Wladyslaw Lokietek", "Mieszko I", "Krak", "Popiel", "Fryderyk August"]
atrybuty     = ["Berlo", "Konstytucja 3 maja", "iPad", "jablko", "korona", "front jednosci narodu", "lista wyborcza", "cenzura", "wstazka", "dwie wstazki", "order pracy ze wstazka", "order pracy bez wstazki", "partia", "narod", "komitet wyborczy", "wyborcy", "lud", "elity", "reputacja", "media", "stronnicze media", "obiektywne media", "praworzadnosc", "promienny usmiech", "usmiech do zlej gry", "kabel", "BOR", "garnitur", "mundur"]




alibManager = LibManager.insNode (0, Library "std"           (UniPath.fromUnixString "lunalib/stdlib" ) 0)
            $ LibManager.insNode (1, Library "__workspace__" (UniPath.fromUnixString "lunalib/project") 100 ) 
            $ LibManager.empty


adefManager = DefManager.addToParentMany (listToDefs atrybuty 2000 20 0)
            $ DefManager.addToParent (0, 20, mkModule "atrybuty" 0)
            $ DefManager.addToParentMany (listToDefs wladcyPolski 1000 10 0)
            $ DefManager.addToParent (0, 10, mkModule "wladcyPolski" 0)
            $ DefManager.insNodes [(0, mkModule "std" 0),
                                   (100, mkModule "user" 1)]
            $ DefManager.empty 


project = Project alibManager adefManager
