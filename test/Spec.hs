import           Align
import           Test.Hspec

main :: IO ()
main = hspec tests

tests :: Spec
tests = describe "Align.align" $ do
    it "Leaves a single line untouched" $
        let t = ["hej,apa,b,c,d,hejsan"]
         in align [Separator ","] t `shouldBe` t
    it "Aligns simple example correctly" $ do
        let input = [ "a,b,c,d"
                    , "hej,hej,b,hopp"
                    , "ab,fluffer,fak,flu"
                    ]
            expected = [ "a  ,b      ,c  ,d"
                       , "hej,hej    ,b  ,hopp"
                       , "ab ,fluffer,fak,flu"
                       ]
        align [Separator ","] input `shouldBe` expected
