{-# LANGUAGE TemplateHaskell #-}

module Estuary.Languages.CineCer0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time
import Data.Text (Text)

import Control.Lens hiding (element)
import Control.Lens.TH

import qualified Estuary.Languages.CineCer0.PositionAndRate as VT

import Estuary.Types.Tempo

import Estuary.Languages.CineCer0.TH

data VideoSpec = VideoSpec {
  _sampleVideo :: Text,
  _sourceNumber :: Int,
  _playbackPosition :: Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime,
  _playbackRate :: Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational,
  --mask :: String,
  _posX :: Rational,
  _posY :: Rational,
  _width :: Rational,
  _height :: Rational,
  _red :: Rational,
  _green :: Rational,
  _blue :: Rational,
  _alpha :: Rational,
  _hue :: Rational,
  _saturation :: Rational
  }




-- data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

-- data Point = Point { _x :: Double, _y :: Double } deriving (Show)

-- lensHack "Atom"

-- lensHack "Point"

-- lensHack "VideoSpec"

instance Show VideoSpec where
  show (VideoSpec vs n _ _ px py w h r g b a _ _) = "Sample Video:" ++ show vs ++ " " ++ "Source Number:" ++ show n ++ " " ++ "Position:" ++ show px ++ show py ++ " " ++ "Size:" ++ show w ++ show h ++ " " ++ "Color:" ++ show r ++ show g ++ show b ++ " " ++ "Alpha " ++ show a


textToVideoSpec :: Text -> VideoSpec
textToVideoSpec x = VideoSpec {
  _sampleVideo = x,
  _sourceNumber = 0,
  _playbackPosition = VT.playNatural_Pos 0.0,
  _playbackRate = VT.playNatural_Rate 0.0,
  --mask = "none"
  _posX = 0.0,
  _posY = 0.0,
  _width = 1.0,
  _height = 1.0,
  _red = 1.0,
  _green = 1.0,
  _blue = 1.0,
  _alpha = 1.0,
  _hue = 0.0,
  _saturation = 0.0
}



--
-- Time Functions --

{- playNatural :: Rational -> VideoSpec -> VideoSpec
playNatural n = (set playbackPosition $ VT.playNatural_Pos n) .
  (set playBackRate $ VT.playNatural_Rate n) -}

playEvery :: Rational -> Rational -> VideoSpec -> VideoSpec
playEvery m n vs = vs {
  _playbackPosition = VT.playEvery_Pos m n,
  _playbackRate = VT.playEvery_Rate m n
  }

playRound :: Rational -> VideoSpec -> VideoSpec
playRound n vs = vs {
  _playbackPosition = VT.playRound_Pos n,
  _playbackRate = VT.playRound_Rate n
  }

playChop' :: Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop' l m n vs = vs {
  _playbackPosition = VT.playChop_Pos' l m n,
  _playbackRate = VT.playChop_Rate' l m n
  }

playChop :: Rational -> Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop k l m n vs = vs {
  _playbackPosition = VT.playChop_Pos k l m n,
  _playbackRate = VT.playChop_Rate k l m n
}

playChopSecs :: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec
playChopSecs k l m n vs = vs {
  _playbackPosition = VT.playChopSecs_Pos k l m n,
  _playbackRate = VT.playChopSecs_Rate k l m n
  }

playNow :: NominalDiffTime -> Rational -> VideoSpec -> VideoSpec
playNow m n vs = vs {
  _playbackPosition = VT.playNow_Pos m n,
  _playbackRate = VT.playNow_Rate m n
  }
