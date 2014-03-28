import           Data.ByteString      as BS
import           Data.ByteString.Lazy as LBS
import qualified Text.Doc.Markup      as Markup


code :: String
code = unlines [ ""
                         -- , "{{font}}"
                         , "=== Heading1 ==="
                         , "=== Heading1 ==="
                         , "== Heading2 =="
                         , "= Heading3 ="
                         , "== Heading2 =="
                         , "= Heading3 ="
                         , "= Heading3 ="
                         , "=== Heading1 ==="
                         , "= Heading3 ="
                         , ""
                         , "    Kod zaczyna sie po 4 wcieciach"
                         , "    Jezeli taki blok poprzedzała linia zawierajaca tylko biale znaki"
                         , "    Po zakonczeniu bloku kodu tez powinna byc pusta linia"
                         , ""
                         , "    {{lang_hs}}"
                         , "    fibs :: [Int]"
                         , "    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)"
                         , ""
                         , "wspierane tagi języków {{lang-c}}"
                         , ""
                         , "    lang-go      lang-mumps   lang-sql     lang-yaml"
                         , "    lang-apollo  lang-hs      lang-n       lang-tcl"
                         , "    lang-basic   lang-lisp    lang-pascal  lang-tex "
                         , "    lang-clj     lang-llvm    lang-proto   lang-vb "
                         , "    lang-css     lang-lua     lang-rd      lang-vhdl"
                         , "    lang-dart    lang-matlab  lang-r       lang-wiki"
                         , "    lang-erlang  lang-ml      lang-scala   lang-xq"
                         , ""
                         , "Kod inline zamieszcza sie pomiedzy baktikami `jak np tu`"
                         , ""
                         , "dla testu sprawdzimy > cytaty wewnątrz tekstu :D"
                         , "oraz    kod wewnątrz tekstu"
                         , ""
                         , "> Cytaty rozpoczynaja sie od ptaszkow"
                         , "> Cytaty rozpoczynaja sie od ptaszkow"
                         , "> Cytaty rozpoczynaja sie od ptaszkow"
                         , "> Cytaty rozpoczynaja sie od ptaszkow"
                         , ""
                         , "Pogrubiony tekst znajduje sie pomiedzy *pojedynczymi gwaizdkami*"
                         , "Kursywa znajduje sie pomiedzy **podwojnymi gwiazdkami**"
                         , "Pogrubienie i kursywa znajduja sie pomiedzy ***potrojnymi gwiazdkami***"
                         , ""
                         , "Linki moga byc zapisane tak [Nazwa linku](http://link)"
                         , "Albo tak: [Nazwa linku][1]"
                         , "A to link wewnątrz dokumentu [Heading1](#Heading1)"
                         , "a na samym dole dokumentu znajduja sie rozwiniecia linkow, jak np:"
                         , "[1]: http:/link"
                         , ""
                         , "Jezeli w tekscie znajduje sie link, jak np tu http://google.pl to rowniez jest zamieniany na element klikalny"
                         , ""
                         , "Potrojna pauza to horizontal rule"
                         , "---"
                         , ""
                         , "Listy bullet zaczynaja sie od pauz ze spacja po nich"
                         , "- jakas lista elementow"
                         , "- inny element"
                         , "    - zagniezdzony element"
                         , ""
                         , ""
                         , "Listy numerowane zaczynaja sie znakiem #. Zagniezdzone zamieniaja sie na numerowania a - z:"
                         , "# pierwszy element"
                         , "# drugi element"
                         , "    # element 2.a"
                         , ""
                         , "Obrazki zamieszcza sie jak linki, tlyko z wykrzyknikiem na poczatku:"
                         , "![Valid XHTML](http://w3.org/Icons/valid-xhtml10)."
                         , "W wersji zaawansowanej mozemy wspierac tagi html w tekscie."
                         ]

code_test = unlines [ 
                      ""
                    , "    {{haskell}}"
                    , "    ala"
                    , "    s"
                    , "    wspaniały kod"
                    ]

main :: IO ()
main = do
          --print $ Markup.parse_test code_test
          case Markup.parse code of
               Left parseError -> print parseError
               Right parsed -> do print parsed; BS.writeFile "test.html" $ lazyToStrictBS parsed
     
--lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x
