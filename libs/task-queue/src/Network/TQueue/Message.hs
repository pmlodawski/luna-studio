module Network.Tqueue.Message where



data MessageType = Query
                 | Free



data Message (t :: MessageType) ...

type family ResponseFor q


data GetSlaves

type instance ResponseFor GetSlaves [Slave]


-- timeout!
-- slavy wykonuja w watkach polecenia - jak jest crash - to zwracamy crash



send (... :: Message Free ...)


-- do kogo message - np chcemy odpowiedzi od wszystkich
-- sprawdzanie ile workerow jest zywych

-- UWAGA zadania sie kolejkuja.

-- parowane messegy - wsyylamy jakies pytanie o cos alem ozemy miedzyczasie dostac inne


message = topic, msg  


-- moze byc wiele klientow na tym samym adressie i porcie!
-- unifikowany format wiadomosci z bledami - np . nie zrozumialem
-- fajnie dodawac wlasne bledy - np. nie udalo sie sparsowa protobufa
-- TOPIC musi byc pierwszy!

-- topic moze byc datatypem - show -> bytestring
-- wysylanie wielu ramek - jako progress, oznaczenie ostatniej ramki

-- corelationID - watek rozmowy (id pytajacego, id wymyslone, unikalne przez pytajacego)

-- server do przydzielania id  

    <- [7,1] 7
    -> [7,1] 5 -- 5 odpowiedziala
    -> [7,1] 3 -- 3 odpowiedziala

-- laczenie kazdy z kazdym lub z hubem na srodku - na razie hub, alee pomyslec o generalizacji