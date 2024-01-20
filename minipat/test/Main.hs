{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Bowtie (pattern JotP)
import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)
import Looksee (Err, parse)
import Minipat.Ast -- TODO qualify
import Minipat.Base (Arc (..), Ev (..), Span (..), patRun)
import Minipat.Interp (interpPat)
import Minipat.Norm (normPat)
import Minipat.Parser (P, ParseErr, factorP, identP, identPatP)
import Minipat.Print (render)
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
  enc = pure . render
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

mkTPat :: PatF (TPat Factor) a (UnPat () a) -> TPat a
mkTPat = Pat . JotP ()

mkUnTPat :: PatF (TPat Factor) a (UnPat () a) -> UnTPat a
mkUnTPat = unPat . mkTPat

tpatP :: P (TPat Ident)
tpatP = fmap (first (const ())) identPatP

neseq :: [a] -> NESeq a
neseq = NESeq.unsafeFromSeq . Seq.fromList

xPatIdent, yPatIdent :: UnTPat Ident
xPatIdent = mkUnTPat (PatPure (Ident "x"))
yPatIdent = mkUnTPat (PatPure (Ident "y"))

xyPatIdents, zwPatIdents :: NESeq (UnTPat Ident)
xyPatIdents = neseq (fmap (mkUnTPat . PatPure) [Ident "x", Ident "y"])
zwPatIdents = neseq (fmap (mkUnTPat . PatPure) [Ident "z", Ident "w"])

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
        (mkTPat (PatTime (TimeShort ShortTimeElongate)))
    , mkUnitRT
        "pat short replicate"
        (expectText "!" (expectParseOk tpatP))
        (mkTPat (PatTime (TimeShort ShortTimeReplicate)))
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
                (PatGroup (Group 0 (GroupTypeSeq SeqPresDot) (NESeq.unsafeFromSeq (Seq.fromList [p xyPatIdents, p zwPatIdents]))))
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
        (mkTPat (PatPoly (PolyPat xyPatIdents Nothing)))
    , mkUnitRT
        "pat poly div"
        (expectText "{x , y}%7" (expectParseOk tpatP))
        (mkTPat (PatPoly (PolyPat xyPatIdents (Just 7))))
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
        (mkTPat (PatTime (TimeLong xPatIdent (LongTimeElongate (FactorRational RationalPresFrac (3 % 7))))))
    , mkUnitRT
        "pat long replicate"
        (expectText "x!5" (expectParseOk tpatP))
        (mkTPat (PatTime (TimeLong xPatIdent (LongTimeReplicate (Just 5)))))
    , mkUnitRT
        "pat long replicate implicit"
        (expectText "x!" (expectParseOk tpatP))
        (mkTPat (PatTime (TimeLong xPatIdent (LongTimeReplicate Nothing))))
    , mkUnitRT
        "pat adj optional implicit"
        (expectText "x?" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeDegrade (Degrade Nothing)))))
    , mkUnitRT
        "pat adj optional explicit"
        (expectText "x?0.5" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeDegrade (Degrade (Just (FactorRational RationalPresDec (1 % 2))))))))
    , mkUnitRT
        "pat adj euclid 2"
        (expectText "x(1,2)" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeEuclid (Euclid 1 2 Nothing)))))
    , mkUnitRT
        "pat adj euclid 3"
        (expectText "x(1,2,3)" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeEuclid (Euclid 1 2 (Just 3))))))
    , mkUnitRT
        "pat adj select sample"
        (expectText "x:4" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeSelect (SelectSample 4)))))
    , mkUnitRT
        "pat adj select transform"
        (expectText "x:hello" (expectParseOk tpatP))
        (mkTPat (PatMod (Mod xPatIdent (ModTypeSelect (SelectTransform "hello")))))
    , mkUnitRT
        "pat multi mod"
        (expectText "x:foo(1,2)" (expectParseOk tpatP))
        ( let p = mkUnTPat (PatMod (Mod xPatIdent (ModTypeSelect (SelectTransform "foo"))))
          in  mkTPat (PatMod (Mod p (ModTypeEuclid (Euclid 1 2 Nothing))))
        )
    , mkUnitRT
        "pat par multi"
        (expectText "[x y , z w]" (expectParseOk tpatP))
        ( let p vs = mkUnTPat (PatGroup (Group 0 (GroupTypeSeq SeqPresSpace) vs))
          in  mkTPat (PatGroup (Group 1 GroupTypePar (NESeq.unsafeFromSeq (Seq.fromList [p xyPatIdents, p zwPatIdents]))))
        )
    , mkUnitRT
        "pat long replicate implicit seq"
        (expectText "x! y" (expectParseOk tpatP))
        ( mkTPat
            ( PatGroup
                ( Group
                    0
                    (GroupTypeSeq SeqPresSpace)
                    ( neseq
                        [ mkUnTPat (PatTime (TimeLong xPatIdent (LongTimeReplicate Nothing)))
                        , yPatIdent
                        ]
                    )
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
                    ( neseq
                        [ xPatIdent
                        , mkUnTPat (PatTime (TimeShort ShortTimeReplicate))
                        ]
                    )
                )
            )
        )
    ]

runTripCase :: (TestName, Text, Maybe Text) -> TestTree
runTripCase (name, txt, mnorm) = testCase name $ do
  -- print txt
  let e = expectParseOk tpatP
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
  pat <- either throwIO pure (parse tpatP patStr)
  let pat' = normPat pat
  pat' @?= npat

testPatNormCases :: TestTree
testPatNormCases =
  let patPure = JotP () . PatPure
      patTime r l = JotP () (PatTime (TimeLong r l))
  in  testGroup "pat norm cases" $
        fmap
          runPatNormCase
          [
            ( "pure"
            , "x"
            , Pat (patPure "x")
            )
          ,
            ( "seq singleton"
            , "[x]"
            , Pat (patPure "x")
            )
          ,
            ( "seq simple"
            , "[x y]"
            , Pat
                ( JotP
                    ()
                    (PatGroup (Group 1 (GroupTypeSeq SeqPresSpace) (neseq [patPure "x", patPure "y"])))
                )
            )
          ,
            ( "repeat one long"
            , "x!1"
            , Pat (patTime (patPure "x") (LongTimeReplicate (Just 1)))
            )
          ,
            ( "repeat two long"
            , "x!2"
            , Pat (patTime (patPure "x") (LongTimeReplicate (Just 2)))
            )
          ,
            ( "repeat two long implicit"
            , "x!"
            , Pat (patTime (patPure "x") (LongTimeReplicate Nothing))
            )
          ,
            ( "repeat two short"
            , "x !"
            , Pat (patTime (patPure "x") (LongTimeReplicate Nothing))
            )
          ,
            ( "repeat three short"
            , "x ! !"
            , let xpart = patTime (patPure "x") (LongTimeReplicate Nothing)
              in  Pat (patTime xpart (LongTimeReplicate Nothing))
            )
          ,
            ( "repeat seq short"
            , "x ! y"
            , let xpart = patTime (patPure "x") (LongTimeReplicate Nothing)
              in  Pat (JotP () (PatGroup (Group 0 (GroupTypeSeq SeqPresSpace) (neseq [xpart, patPure "y"]))))
            )
          ,
            ( "elongate two long"
            , "x@2"
            , Pat (patTime (patPure "x") (LongTimeElongate 2))
            )
          ,
            ( "elongate two short"
            , "x _"
            , Pat (patTime (patPure "x") (LongTimeElongate 2))
            )
          ]

runPatInterpCase :: (TestName, Maybe Arc, Text, [Ev Ident]) -> TestTree
runPatInterpCase (n, mayArc, patStr, evs) = testCase n $ do
  pat <- either throwIO pure (parse tpatP patStr)
  let pat' = normPat pat
  pat'' <- either throwIO pure =<< interpPat pat'
  let arc = fromMaybe (Arc 0 1) mayArc
      actualEvs = patRun pat'' arc
  actualEvs @?= evs

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
          [ Ev (Span (Arc 0 1) (Just (Arc 0 1))) "x"
          ]
        )
      ,
        ( "pure longer"
        , Just (Arc 0 2)
        , "x"
        ,
          [ Ev (Span (Arc 0 1) (Just (Arc 0 1))) "x"
          , Ev (Span (Arc 1 2) (Just (Arc 1 2))) "x"
          ]
        )
      ,
        ( "pure shift"
        , Just (Arc (1 % 2) (3 % 2))
        , "x"
        ,
          [ Ev (Span (Arc (1 % 2) 1) (Just (Arc 0 1))) "x"
          , Ev (Span (Arc 1 (3 % 2)) (Just (Arc 1 2))) "x"
          ]
        )
      ,
        ( "seq singleton"
        , Nothing
        , "[x]"
        ,
          [ Ev (Span (Arc 0 1) (Just (Arc 0 1))) "x"
          ]
        )
      ,
        ( "speed up"
        , Nothing
        , "x*2"
        ,
          [ Ev (Span (Arc 0 (1 % 2)) (Just (Arc 0 (1 % 2)))) "x"
          , Ev (Span (Arc (1 % 2) 1) (Just (Arc (1 % 2) 1))) "x"
          ]
        )
      , -- ,
        --   ( "slow down"
        --   , Nothing
        --   , "x/2"
        --   , [ Ev (Span (Arc 0 1) (Just (Arc 0 2))) "x"
        --     ]
        --   )

        ( "seq simple"
        , Nothing
        , "[x y]"
        ,
          [ Ev (Span (Arc 0 (1 % 2)) (Just (Arc 0 (1 % 2)))) "x"
          , Ev (Span (Arc (1 % 2) 1) (Just (Arc (1 % 2) 1))) "y"
          ]
        )
        -- ,
        --   ( "seq two cycle"
        --   , Just (mkCtx (Arc 0 2))
        --   , "[x y]"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (1 % 2)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (1 % 2) 1) "y"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc 1 (3 % 2)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (3 % 2) 2) "y"))
        --           ]
        --   )
        -- ,
        --   ( "repeat one long"
        --   , Nothing
        --   , "x!1"
        --   , mkTPatStream (PatStreamPure (Anno (Arc 0 1) "x"))
        --   )
        -- ,
        --   ( "repeat two long"
        --   , Nothing
        --   , "x!2"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (1 % 2)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (1 % 2) 1) "x"))
        --           ]
        --   )
        -- ,
        --   ( "repeat two long implicit"
        --   , Nothing
        --   , "x!"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (1 % 2)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (1 % 2) 1) "x"))
        --           ]
        --   )
        -- ,
        --   ( "repeat two short"
        --   , Nothing
        --   , "x !"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (1 % 2)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (1 % 2) 1) "x"))
        --           ]
        --   )
        -- ,
        --   ( "repeat three long"
        --   , Nothing
        --   , "x!3"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (1 % 3)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (1 % 3) (2 % 3)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (2 % 3) 1) "x"))
        --           ]
        --   )
        -- ,
        --   ( "repeat three short"
        --   , Nothing
        --   , "x ! !"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (1 % 3)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (1 % 3) (2 % 3)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (2 % 3) 1) "x"))
        --           ]
        --   )
        -- ,
        --   ( "repeat seq short"
        --   , Nothing
        --   , "x ! y"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (1 % 3)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (1 % 3) (2 % 3)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (2 % 3) 1) "y"))
        --           ]
        --   )
        -- ,
        --   ( "elongate noop"
        --   , Nothing
        --   , "x@2"
        --   , mkTPatStream (PatStreamPure (Anno (Arc 0 1) "x"))
        --   )
        -- ,
        --   ( "elongate long seq"
        --   , Nothing
        --   , "x@2 y"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (2 % 3)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (2 % 3) 1) "y"))
        --           ]
        --   )
        -- ,
        --   ( "elongate short seq"
        --   , Nothing
        --   , "x _ y"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 (2 % 3)) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc (2 % 3) 1) "y"))
        --           ]
        --   )
        -- ,
        --   ( "rand two"
        --   , Nothing
        --   , "[x | y]"
        --   , mkTPatStream (PatStreamPure (Anno (Arc 0 1) "x"))
        --   )
        -- ,
        --   ( "rand many"
        --   , Just (mkCtx (Arc 5 8))
        --   , "[x | y | z]"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 5 6) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc 6 7) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc 7 8) "y"))
        --           ]
        --   )
        -- ,
        --   ( "alt singleton"
        --   , Nothing
        --   , "<x>"
        --   , mkTPatStream (PatStreamPure (Anno (Arc 0 1) "x"))
        --   )
        -- ,
        --   ( "alt two"
        --   , Nothing
        --   , "<x y>"
        --   , mkTPatStream (PatStreamPure (Anno (Arc 0 1) "x"))
        --   )
        -- ,
        --   ( "alt many"
        --   , Just (mkCtx (Arc 5 8))
        --   , "<x y z>"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypeSeq $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 5 6) "z"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc 6 7) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc 7 8) "y"))
        --           ]
        --   )
        -- ,
        --   ( "par many"
        --   , Nothing
        --   , "[x , y , z]"
        --   , mkTPatStream $
        --       PatStreamBranch PatStreamTypePar $
        --         neseq
        --           [ mkTPatStream (PatStreamPure (Anno (Arc 0 1) "x"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc 0 1) "y"))
        --           , mkTPatStream (PatStreamPure (Anno (Arc 0 1) "z"))
        --           ]
        --   )
      ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  daytripperMain $
    testGroup
      "Minipat"
      [ testParseCases
      , testPatNormCases
      , testPatInterpCases
      ]
