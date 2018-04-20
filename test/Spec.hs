import           Align
import           Test.Hspec

main :: IO ()
main = hspec tests

tests :: Spec
tests = describe "Align.align" $ do
    it "Leaves a single line untouched" $
        let t = ["hej,apa,b,c,d,hejsan"]
         in align (defaultOptions [Separator ","]) t `shouldBe` t
    it "Aligns simple example correctly" $ do
        let input = [ "a,b,c,d"
                    , "hej,hej,b,hopp"
                    , "ab,fluffer,fak,flu"
                    ]
            expected = [ "a  ,b      ,c  ,d"
                       , "hej,hej    ,b  ,hopp"
                       , "ab ,fluffer,fak,flu"
                       ]
        align (defaultOptions [Separator ","]) input `shouldBe` expected
    it "Hanle multiple separators" $ do
        let input = [ "hello there sir"
                    , "a,b,c,d"
                    , "Now,will this work?"
                    ]
            expected = [ "hello there sir"
                       , "a    ,b    ,c   ,d"
                       , "Now  ,will  this work?"
                       ]
        align (defaultOptions [Separator ",", Separator " "]) input `shouldBe` expected
    it "Handle strip before" $ do
        let input = [ "hello , there!"
                    , " One, two  , three"
                    ]
            expected = [ "hello ,there!"
                       , "One   ,two   ,three"
                       ]
        align (defaultOptions [Separator ","]){stripBefore = True} input
            `shouldBe` expected
    it "Handle strip after" $ do
        let input = [ "hello , there!"
                    , " One, two  , three"
                    ]
            expected = [ "hello, there!"
                       , " One , two   , three"
                       ]
        align (defaultOptions [Separator ","]){stripAfter = True} input
            `shouldBe` expected
    it "Handle strip before and after" $ do
        let input = [ "hello , there!"
                    , " One, two  , three "
                    ]
            expected = [ "hello,there!"
                       , "One  ,two   ,three"
                       ]
        align (defaultOptions [Separator ","]){stripAfter = True, stripBefore=True} input
            `shouldBe` expected
