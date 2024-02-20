{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Bowtie (pattern JotP)
import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Either (isLeft, isRight)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Looksee (Err, intP, parse)
import Minipat.Ast
import Minipat.Classes (Pattern (..))
import Minipat.Eval (evalPat)
import Minipat.Interp (interpPat)
import Minipat.Norm (normPat)
import Minipat.Parser (Loc, P, ParseErr, factorP, identP, identPatP, selectIdentPatP)
import Minipat.Print (prettyShow)
import Minipat.Stream (Ev (..), Stream, streamRun, tapeToList)
import Minipat.Time (Arc (..), CycleTime (..), Span (..))
import Minipat.Ur (ur)
import Prettyprinter qualified as P
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Test.Daytripper
  ( Expect
  , MonadExpect (..)
  , daytripperMain
  , expectDuring
  , mkExpect
  , mkUnitRT
  , runExpect
  , testRT
  )
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

type Cmp m a = Maybe a -> Either (Err ParseErr) a -> m ()

expectParse :: (MonadExpect m, P.Pretty a) => Cmp m a -> P a -> Expect m a Text (Either (Err ParseErr) a)
expectParse cmp p = mkExpect enc dec cmp
 where
  enc = pure . prettyShow
  dec = pure . parse p

expectParseOk :: (MonadExpect m, Eq a, Show a, P.Pretty a) => P a -> Expect m a Text (Either (Err ParseErr) a)
expectParseOk = expectParse (maybe (expectAssertBool "expected ok" . isRight) (\a x -> expectAssertEq x (Right a)))

expectParseErr :: (MonadExpect m, P.Pretty a) => P a -> Expect m a Text (Either (Err ParseErr) a)
expectParseErr = expectParse (const (expectAssertBool "expected error" . isLeft))

expectText :: (MonadExpect m) => Text -> Expect m a Text c -> Expect m a Text c
expectText t = expectDuring (\_ u -> expectAssertEq u t)

commonParseTests :: [TestTree]
commonParseTests =
  fmap
    testRT
    [ mkUnitRT "var" (expectText "x" (expectParseOk identP)) (Ident "x")
    , mkUnitRT "factor int" (expectText "5" (expectParseOk factorP)) (FactorInteger 5)
    , mkUnitRT "factor dec" (expectText "0.75" (expectParseOk factorP)) (FactorRational RationalPresDec (3 % 4))
    , mkUnitRT "factor frac" (expectText "(3/4)" (expectParseOk factorP)) (FactorRational RationalPresFrac (3 % 4))
    , mkUnitRT "factor char" (expectText "h" (expectParseOk factorP)) (FactorQuickRatio QuickRatioHalf)
    ]

type TPat = Pat ()

type UnTPat = UnPat ()

mkTPat :: PatF () a (UnPat () a) -> TPat a
mkTPat = Pat . JotP ()

mkUnTPat :: PatF () a (UnPat () a) -> UnTPat a
mkUnTPat = unPat . mkTPat

convTPat :: Pat Loc a -> TPat a
convTPat = first (const ())

tpatP :: P (TPat Ident)
tpatP = fmap convTPat identPatP

tspatP :: P (TPat (Select Integer Ident))
tspatP = fmap convTPat (selectIdentPatP intP)

parsePat :: Text -> IO (Pat Loc Ident)
parsePat = either throwIO pure . parse identPatP

parseStream :: Text -> IO (Stream Ident)
parseStream = either throwIO pure . evalPat identP

parseTPat :: Text -> IO (TPat Ident)
parseTPat = fmap convTPat . parsePat

xPatIdent, yPatIdent :: UnTPat Ident
xPatIdent = mkUnTPat (PatPure (Ident "x"))
yPatIdent = mkUnTPat (PatPure (Ident "y"))

xyPatIdents, zwPatIdents :: Seq (UnTPat Ident)
xyPatIdents = fmap (mkUnTPat . PatPure) [Ident "x", Ident "y"]
zwPatIdents = fmap (mkUnTPat . PatPure) [Ident "z", Ident "w"]

patParseTests :: [TestTree]
patParseTests =
  fmap
    testRT
    [ mkUnitRT
        "pat silence"
        (expectText "~" (expectParseOk tpatP))
        (mkTPat PatSilence)
    , mkUnitRT
        "pat short elongate"
        (expectText "_" (expectParseOk tpatP))
        (mkTPat (PatShort ShortElongate))
    , mkUnitRT
        "pat short replicate"
        (expectText "!" (expectParseOk tpatP))
        (mkTPat (PatShort ShortReplicate))
    , mkUnitRT
        "pat var"
        (expectText "x" (expectParseOk tpatP))
        (Pat xPatIdent)
    , mkUnitRT
        "pat seq brace"
        (expectText "[x y]" (expectParseOk tpatP))
        (mkTPat (PatGroup (Group 1 (GroupTypeSeq SeqPresSpace) xyPatIdents)))
    , mkUnitRT
        "pat seq space"
        (expectText "x y" (expectParseOk tpatP))
        (mkTPat (PatGroup (Group 0 (GroupTypeSeq SeqPresSpace) xyPatIdents)))
    , mkUnitRT
        "pat seq dot"
        (expectText "x . y" (expectParseOk tpatP))
        (mkTPat (PatGroup (Group 0 (GroupTypeSeq SeqPresDot) xyPatIdents)))
    , mkUnitRT
        "pat seq dot space"
        (expectText "x y . z w" (expectParseOk tpatP))
        ( let p vs = mkUnTPat (PatGroup (Group 0 (GroupTypeSeq SeqPresSpace) vs))
          in  mkTPat
                (PatGroup (Group 0 (GroupTypeSeq SeqPresDot) [p xyPatIdents, p zwPatIdents]))
        )
    , mkUnitRT
        "pat par"
        (expectText "[x , y]" (expectParseOk tpatP))
        (mkTPat (PatGroup (Group 1 GroupTypePar xyPatIdents)))
    , mkUnitRT
        "pat alt"
        (expectText "<x y>" (expectParseOk tpatP))
        (mkTPat (PatGroup (Group 0 GroupTypeAlt xyPatIdents)))
    , mkUnitRT
        "pat rand"
        (expectText "[x | y]" (expectParseOk tpatP))
        (mkTPat (PatGroup (Group 1 GroupTypeRand xyPatIdents)))
    , mkUnitRT
        "pat poly"
        (expectText "{x , y}" (expectParseOk tpatP))
        (mkTPat (PatPoly (Poly xyPatIdents Nothing)))
    , mkUnitRT
        "pat poly div"
        (expectText "{x , y}%7" (expectParseOk tpatP))
        (mkTPat (PatPoly (Poly xyPatIdents (Just 7))))
    , mkUnitRT
        "pat speed fast"
        (expectText "x*9" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeSpeed (Speed SpeedDirFast (mkTPat (PatPure (FactorInteger 9))))))))
    , mkUnitRT
        "pat speed fast decimal"
        (expectText "x*9.2" (expectParseOk tpatP))
        ( mkTPat
            ( PatMod (Mod xPatIdent (ModTypeSpeed (Speed SpeedDirFast (mkTPat (PatPure (FactorRational RationalPresDec (92 % 10)))))))
            )
        )
    , mkUnitRT
        "pat speed slow"
        (expectText "x/(1/3)" (expectParseOk tpatP))
        ( mkTPat
            ( PatMod (Mod xPatIdent (ModTypeSpeed (Speed SpeedDirSlow (mkTPat (PatPure (FactorRational RationalPresFrac (1 % 3)))))))
            )
        )
    , mkUnitRT
        "pat long elongate"
        (expectText "x@(3/7)" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeElongate (Elongate (FactorRational RationalPresFrac (3 % 7)))))))
    , mkUnitRT
        "pat long replicate"
        (expectText "x!5" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeReplicate (Replicate (Just 5))))))
    , mkUnitRT
        "pat long replicate implicit"
        (expectText "x!" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeReplicate (Replicate Nothing)))))
    , mkUnitRT
        "pat adj optional implicit"
        (expectText "x?" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeDegrade (Degrade Nothing)))))
    , mkUnitRT
        "pat adj optional explicit"
        (expectText "x?0.5" (expectParseOk tpatP))
        ( mkTPat
            (PatMod (Mod xPatIdent (ModTypeDegrade (Degrade (Just (mkTPat (PatPure (FactorRational RationalPresDec (1 % 2)))))))))
        )
    , mkUnitRT
        "pat adj euclid 2"
        (expectText "x(1,2)" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeEuclid (Euclid 1 2 Nothing)))))
    , mkUnitRT
        "pat adj euclid 3"
        (expectText "x(1,2,3)" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeEuclid (Euclid 1 2 (Just 3))))))
    , mkUnitRT
        "pat par multi"
        (expectText "[x y , z w]" (expectParseOk tpatP))
        ( let p vs = mkUnTPat (PatGroup (Group 0 (GroupTypeSeq SeqPresSpace) vs))
          in  mkTPat (PatGroup (Group 1 GroupTypePar [p xyPatIdents, p zwPatIdents]))
        )
    , mkUnitRT
        "pat long replicate implicit seq"
        (expectText "x! y" (expectParseOk tpatP))
        ( mkTPat
            ( PatGroup
                ( Group
                    0
                    (GroupTypeSeq SeqPresSpace)
                    [ mkUnTPat (PatMod (Mod xPatIdent (ModTypeReplicate (Replicate Nothing))))
                    , yPatIdent
                    ]
                )
            )
        )
    , mkUnitRT
        "pat short replicate seq"
        (expectText "x !" (expectParseOk tpatP))
        ( mkTPat
            ( PatGroup
                ( Group
                    0
                    (GroupTypeSeq SeqPresSpace)
                    [ xPatIdent
                    , mkUnTPat (PatShort ShortReplicate)
                    ]
                )
            )
        )
    ]

runTripCase :: (TestName, Text, Maybe Text) -> TestTree
runTripCase (name, txt, mnorm) = testCase name $ do
  -- print txt
  let e = expectParseOk tspatP
  (_, act) <- e (Left txt)
  ea <- act
  -- pPrint ea
  case ea of
    Left err -> throwIO err
    Right a ->
      let etxt = fromMaybe txt mnorm
      in  void (runExpect (expectText etxt e) a)

patTripTests :: [TestTree]
patTripTests =
  fmap
    runTripCase
    [ ("ex00", "x", Nothing)
    , ("ex01", "~ hh", Nothing)
    , ("ex02", "[bd sd] hh", Nothing)
    , ("ex03", "bd sd . hh hh hh", Nothing)
    ,
      ( "ex04"
      , "[bd sd, hh hh hh]"
      , Just "[bd sd , hh hh hh]"
      )
    , ("ex05", "bd*2 sd", Nothing)
    , ("ex06", "bd/2", Nothing)
    ,
      ( "ex07"
      , "[bd |cp |hh]"
      , Just "[bd | cp | hh]"
      )
    , ("ex08", "bd <sd hh cp>", Nothing)
    , ("ex09", "bd!3 sd", Nothing)
    , ("ex10", "bd _ _ ~ sd _", Nothing)
    , ("ex11", "superpiano@3 superpiano", Nothing)
    , ("ex12", "bd? sd", Nothing)
    , ("ex13", "bd:3", Nothing)
    , ("ex14", "bd(3,8)", Nothing)
    ,
      ( "ex15"
      , "{bd bd bd bd, cp cp hh}"
      , Just "{bd bd bd bd , cp cp hh}"
      )
    , ("ex16", "{bd cp hh}%8", Nothing)
    , ("ex17", "[bd [hh [cp sn:2] hh]] bd bd bd", Nothing)
    , ("ex18", "bd*3 . hh*4 cp", Nothing)
    , ("ex19", "[bd*3 . hh:2*2] . hh*4 cp", Nothing)
    ,
      ( "ex20"
      , "[bd*3,hh*4,[~ cp]*2, bass*4]"
      , Just "[bd*3 , hh*4 , [~ cp]*2 , bass*4]"
      )
    , ("ex21", "[[c3*3],[c e g c6*3]]", Just "[[c3*3] , [c e g c6*3]]")
    , ("ex22", "cp cp cp*2", Nothing)
    , ("ex23", "bd <[hh sn] [hh cp]>", Nothing)
    , ("ex24", "bd hh? bd hh?0.8", Nothing)
    ,
      ( "ex25"
      , "[bd*4, [arpy:1,arpy:2,arpy:3,arpy:4,arpy:5](5,8)]"
      , Just "[bd*4 , [arpy:1 , arpy:2 , arpy:3 , arpy:4 , arpy:5](5,8)]"
      )
    ,
      ( "ex26"
      , "[bd(5,8), cp(1,8)?, hh(7,8), bass:1(8,16)]"
      , Just "[bd(5,8) , cp(1,8)? , hh(7,8) , bass:1(8,16)]"
      )
    ,
      ( "ex27"
      , "{bd*2, hh*4, bd hh 808:4}"
      , Just "{bd*2 , hh*4 , bd hh 808:4}"
      )
    , ("ex28", "{bd cp 808:5}%4", Nothing)
    , ("ex29", "x! y", Nothing)
    , ("ex30", "x !", Nothing)
    , ("ex31", "x@2 y", Nothing)
    , ("ex32", "x _", Nothing)
    ]

testParseCases :: TestTree
testParseCases =
  testGroup
    "parse cases"
    [ testGroup "common" commonParseTests
    , testGroup "pat parse" patParseTests
    , testGroup "pat trip" patTripTests
    ]

runPatNormCase :: (TestName, Text, Pat () Ident) -> TestTree
runPatNormCase (n, patStr, npat) = testCase n $ do
  pat <- parseTPat patStr
  let pat' = normPat pat
  pat' @?= npat

testPatNormCases :: TestTree
testPatNormCases =
  let mkPure = JotP () . PatPure
      mkMod r l = JotP () (PatMod (Mod r l))
  in  testGroup "pat norm cases" $
        fmap
          runPatNormCase
          [
            ( "pure"
            , "x"
            , Pat (mkPure "x")
            )
          ,
            ( "seq singleton"
            , "[x]"
            , Pat (mkPure "x")
            )
          ,
            ( "seq simple"
            , "[x y]"
            , Pat
                ( JotP
                    ()
                    (PatGroup (Group 1 (GroupTypeSeq SeqPresSpace) [mkPure "x", mkPure "y"]))
                )
            )
          ,
            ( "repeat one long"
            , "x!1"
            , Pat (mkMod (mkPure "x") (ModTypeReplicate (Replicate (Just 1))))
            )
          ,
            ( "repeat two long"
            , "x!2"
            , Pat (mkMod (mkPure "x") (ModTypeReplicate (Replicate (Just 2))))
            )
          ,
            ( "repeat two long implicit"
            , "x!"
            , Pat (mkMod (mkPure "x") (ModTypeReplicate (Replicate Nothing)))
            )
          ,
            ( "repeat two short"
            , "x !"
            , Pat (mkMod (mkPure "x") (ModTypeReplicate (Replicate Nothing)))
            )
          ,
            ( "repeat three short"
            , "x ! !"
            , Pat (mkMod (mkPure "x") (ModTypeReplicate (Replicate (Just 3))))
            )
          ,
            ( "repeat seq short"
            , "x ! y"
            , let xpart = mkMod (mkPure "x") (ModTypeReplicate (Replicate Nothing))
              in  Pat (JotP () (PatGroup (Group 0 (GroupTypeSeq SeqPresSpace) [xpart, mkPure "y"])))
            )
          ,
            ( "elongate two long"
            , "x@2"
            , Pat (mkMod (mkPure "x") (ModTypeElongate (Elongate 2)))
            )
          ,
            ( "elongate two short"
            , "x _"
            , Pat (mkMod (mkPure "x") (ModTypeElongate (Elongate 2)))
            )
          ]

runPatInterpCase :: (TestName, Maybe Arc, Text, [Ev Ident]) -> TestTree
runPatInterpCase (n, mayArc, patStr, evs) = testCase n $ do
  pat <- either throwIO pure (evalPat identP patStr)
  let arc = fromMaybe (Arc 0 1) mayArc
      actualEvs = tapeToList (streamRun pat arc)
  actualEvs @?= evs

ev :: Rational -> Rational -> x -> Ev x
ev start end val =
  let arc = Arc (CycleTime start) (CycleTime end)
  in  Ev (Span arc (Just arc)) val

testPatInterpCases :: TestTree
testPatInterpCases =
  testGroup "pat interp cases" $
    fmap
      runPatInterpCase
      [
        ( "pure"
        , Nothing
        , "x"
        ,
          [ ev 0 1 "x"
          ]
        )
      ,
        ( "pure longer"
        , Just (Arc 0 2)
        , "x"
        ,
          [ ev 0 1 "x"
          , ev 1 2 "x"
          ]
        )
      ,
        ( "pure shift"
        , Just (Arc (CycleTime (1 % 2)) (CycleTime (3 % 2)))
        , "x"
        ,
          [ Ev (Span (Arc (CycleTime (1 % 2)) 1) (Just (Arc 0 1))) "x"
          , Ev (Span (Arc 1 (CycleTime (3 % 2))) (Just (Arc 1 2))) "x"
          ]
        )
      ,
        ( "seq singleton"
        , Nothing
        , "[x]"
        ,
          [ ev 0 1 "x"
          ]
        )
      ,
        ( "speed up"
        , Nothing
        , "x*2"
        ,
          [ ev 0 (1 % 2) "x"
          , ev (1 % 2) 1 "x"
          ]
        )
      ,
        ( "slow down"
        , Nothing
        , "x/2"
        ,
          [ Ev (Span (Arc 0 1) (Just (Arc 0 2))) "x"
          ]
        )
      ,
        ( "seq simple"
        , Nothing
        , "[x y]"
        ,
          [ ev 0 (1 % 2) "x"
          , ev (1 % 2) 1 "y"
          ]
        )
      ,
        ( "seq two cycle"
        , Just (Arc 0 2)
        , "[x y]"
        ,
          [ ev 0 (1 % 2) "x"
          , ev (1 % 2) 1 "y"
          , ev 1 (3 % 2) "x"
          , ev (3 % 2) 2 "y"
          ]
        )
      ,
        ( "repeat one long"
        , Nothing
        , "x!1"
        ,
          [ ev 0 1 "x"
          ]
        )
      ,
        ( "repeat two long"
        , Nothing
        , "x!2"
        ,
          [ ev 0 (1 % 2) "x"
          , ev (1 % 2) 1 "x"
          ]
        )
      ,
        ( "repeat two long implicit"
        , Nothing
        , "x!"
        ,
          [ ev 0 (1 % 2) "x"
          , ev (1 % 2) 1 "x"
          ]
        )
      ,
        ( "repeat two short"
        , Nothing
        , "x !"
        ,
          [ ev 0 (1 % 2) "x"
          , ev (1 % 2) 1 "x"
          ]
        )
      ,
        ( "repeat three long"
        , Nothing
        , "x!3"
        ,
          [ ev 0 (1 % 3) "x"
          , ev (1 % 3) (2 % 3) "x"
          , ev (2 % 3) 1 "x"
          ]
        )
      ,
        ( "repeat three short"
        , Nothing
        , "x ! !"
        ,
          [ ev 0 (1 % 3) "x"
          , ev (1 % 3) (2 % 3) "x"
          , ev (2 % 3) 1 "x"
          ]
        )
      ,
        ( "repeat seq short"
        , Nothing
        , "x ! y"
        ,
          [ ev 0 (1 % 3) "x"
          , ev (1 % 3) (2 % 3) "x"
          , ev (2 % 3) 1 "y"
          ]
        )
      ,
        ( "elongate noop"
        , Nothing
        , "x@2"
        ,
          [ ev 0 1 "x"
          ]
        )
      ,
        ( "elongate long seq"
        , Nothing
        , "x@2 y"
        ,
          [ ev 0 (2 % 3) "x"
          , ev (2 % 3) 1 "y"
          ]
        )
      ,
        ( "elongate short seq"
        , Nothing
        , "x _ y"
        ,
          [ ev 0 (2 % 3) "x"
          , ev (2 % 3) 1 "y"
          ]
        )
      ,
        ( "rand two"
        , Nothing
        , "[x | y]"
        ,
          [ ev 0 1 "x"
          ]
        )
      ,
        ( "rand many"
        , Just (Arc 5 8)
        , "[x | y | z]"
        ,
          [ ev 5 6 "x"
          , ev 6 7 "x"
          , ev 7 8 "y"
          ] -- Arbitrary, based on rand seed
        )
      ,
        ( "alt singleton"
        , Nothing
        , "<x>"
        ,
          [ ev 0 1 "x"
          ]
        )
      ,
        ( "alt two"
        , Nothing
        , "<x y>"
        ,
          [ ev 0 1 "x"
          ]
        )
      ,
        ( "alt many"
        , Just (Arc 5 8)
        , "<x y z>"
        ,
          [ ev 5 6 "z"
          , ev 6 7 "x"
          , ev 7 8 "y"
          ]
        )
      ,
        ( "par many"
        , Nothing
        , "[x , y , z]"
        ,
          [ ev 0 1 "x"
          , ev 0 1 "y"
          , ev 0 1 "z"
          ] -- Note this order is arbitrary, just comes from heap behavior
        )
      ,
        ( "degrade"
        , Just (Arc 0 4)
        , "x?"
        ,
          [ ev 0 1 "x"
          , ev 1 2 "x"
          ]
        )
      ,
        ( "euclid (3,8)"
        , Nothing
        , "x(3,8)"
        ,
          [ ev 0 (1 % 8) "x"
          , ev (3 % 8) (4 % 8) "x"
          , ev (6 % 8) (7 % 8) "x"
          ]
        )
      ,
        ( "euclid (3,8,1)"
        , Nothing
        , "x(3,8,1)"
        ,
          [ ev (2 % 8) (3 % 8) "x"
          , ev (5 % 8) (6 % 8) "x"
          , ev (7 % 8) (8 % 8) "x"
          ]
        )
      ,
        ( "euclid (3,8,2)"
        , Nothing
        , "x(3,8,2)"
        ,
          [ ev (1 % 8) (2 % 8) "x"
          , ev (4 % 8) (5 % 8) "x"
          , ev (6 % 8) (7 % 8) "x"
          ]
        )
      ,
        ( "euclid (3,8,3)"
        , Nothing
        , "x(3,8,3)"
        ,
          [ ev 0 (1 % 8) "x"
          , ev (3 % 8) (4 % 8) "x"
          , ev (5 % 8) (6 % 8) "x"
          ]
        )
      ]

runPatReprCase :: (TestName, Text, Maybe (TPat Ident), Maybe Text) -> TestTree
runPatReprCase (n, patStr, mayRePat, mayReStr) = testCase n $ do
  pat :: Pat Loc Ident <- either throwIO pure (evalPat identP patStr)
  actualPat :: Pat Loc Ident <- either throwIO pure (interpPat pat)
  for_ mayRePat (first (const ()) actualPat @?=)
  let actualStr = prettyShow actualPat
      expectStr = fromMaybe patStr mayReStr
  actualStr @?= expectStr

testPatReprCases :: TestTree
testPatReprCases =
  testGroup "pat repr cases" $
    fmap
      runPatReprCase
      [
        ( "pure"
        , "x"
        , Just (mkTPat (PatPure "x"))
        , Nothing
        )
      ,
        ( "seq 1"
        , "[x]"
        , Just (mkTPat (PatPure "x"))
        , Just "x"
        )
      ,
        ( "seq 2"
        , "[x y]"
        , Nothing
        , Nothing
        )
      ,
        ( "fast"
        , "x*2"
        , Nothing
        , Nothing
        )
      ,
        ( "slow"
        , "x/2"
        , Nothing
        , Nothing
        )
      ,
        ( "seq fast"
        , "[x*2 y]"
        , Nothing
        , Nothing
        )
      ]

newtype Xform = Xform {unXform :: forall f. (Pattern f) => f Ident -> f Ident}

runXform :: (Pattern f) => Xform -> f Ident -> f Ident
runXform (Xform f) = f

runUrCase
  :: (TestName, Integer, Text, [(Ident, Text)], [(Ident, Xform)], Maybe Text, [Ev Ident])
  -> TestTree
runUrCase (n, inpLen, inpPat, inpSubPats, inpSubXforms, mayExpectStr, expectEvs) = testCase n $ do
  inpSubPats' <- traverse (bitraverse pure parsePat) inpSubPats
  let inpSubXforms' = fmap (second runXform) inpSubXforms
  actualPat :: Pat Loc Ident <- either throwIO pure (ur (fromInteger inpLen) inpPat inpSubPats' inpSubXforms')
  for_ mayExpectStr $ \expectStr -> do
    let actualStr = prettyShow actualPat
    actualStr @?= expectStr
  inpSubPats'' <- traverse (bitraverse pure parseStream) inpSubPats
  let inpSubXforms'' = fmap (second runXform) inpSubXforms
  actualStream :: Stream Ident <- either throwIO pure (ur (fromInteger inpLen) inpPat inpSubPats'' inpSubXforms'')
  let arc = Arc 0 (fromInteger inpLen)
      actualEvs = tapeToList (streamRun actualStream arc)
  actualEvs @?= expectEvs

testUr :: TestTree
testUr =
  testGroup "ur" $
    fmap
      runUrCase
      [
        ( "simple"
        , 1
        , "a"
        , [("a", "x")]
        , []
        , Just "x"
        ,
          [ ev 0 1 "x"
          ]
        )
      ,
        ( "double"
        , 2
        , "a"
        , [("a", "x")]
        , []
        , Just "x/2"
        ,
          [ ev 0 2 "x"
          ]
        )
      ,
        ( "more"
        , 1
        , "a b:x"
        , [("a", "x"), ("b", "y")]
        , [("x", Xform (patFastBy 2))]
        , Just "[x y*2]"
        ,
          [ ev 0 (1 % 2) "x"
          , ev (1 % 2) (3 % 4) "y"
          , ev (3 % 4) 1 "y"
          ]
        )
      ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  daytripperMain $
    testGroup
      "minipat"
      [ testParseCases
      , testPatNormCases
      , testPatInterpCases
      , testPatReprCases
      , testUr
      ]
