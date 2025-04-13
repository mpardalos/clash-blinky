{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Top where

import Clash.Explicit.Prelude (noReset)
import Clash.Prelude

createDomain vSystem {vName = "Dom25", vPeriod = hzToPeriod 25e6}

type Leds = BitVector 8

type Buttons = BitVector 7

-- | @topEntity@ is Clash@s equivalent of @main@ in other programming languages.
-- Clash will look for it when compiling "Example.Project" and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a @topEntity@
-- must be monomorphic and must use non- recursive types. Or, to put it
-- hand-wavily, a @topEntity@ must be translatable to a static number of wires.
--
-- Top entities must be monomorphic, meaning we have to specify all type variables.
-- In this case, we are using the @Dom50@ domain, which we created with @createDomain@
-- and we are using 8-bit unsigned numbers.
topEntity ::
  "clk_25mhz" ::: Clock Dom25 ->
  "btn" ::: Signal Dom25 Buttons ->
  "led" ::: Signal Dom25 Leds
topEntity clk = exposeClockResetEnable blinky clk noReset enableGen
-- -- To specify the names of the ports of our top entity, we create a @Synthesize@ annotation.
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "topEntity",
        t_inputs =
          [ PortName "clk_25mhz",
            PortName "btn"
          ],
        t_output = PortName "led"
      }
  )
  #-}
-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}

slowClk :: forall n dom. (HiddenClockResetEnable dom, KnownNat n) => SNat n -> Signal dom Bool
slowClk _ =
  let counter = register (0 :: Unsigned n) (counter + 1)
   in (== 0) <$> counter

data Module
  = Clicky
  | Counter
  | Bounce
  deriving (Generic, NFDataX)

data ModuleSignals dom
  = ModuleSignals
  { clicky :: !(Signal dom Leds),
    counter :: !(Signal dom Leds),
    bounce :: !(Signal dom Leds)
  }

moduleMux :: Signal dom Module -> ModuleSignals dom -> Signal dom Leds
moduleMux sel ModuleSignals {..} = do
  selectVal <- sel
  clickyVal <- clicky
  counterVal <- counter
  bounceVal <- bounce
  pure $ case selectVal of
    Clicky -> clickyVal
    Counter -> counterVal
    Bounce -> bounceVal

blinky ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom Buttons ->
  Signal dom Leds
blinky btn = moduleMux sel ModuleSignals {..}
  where
    up = detectRisingEdge ((`testBit` 3) <$> btn)
    down = detectRisingEdge ((`testBit` 4) <$> btn)
    reset = detectRisingEdge ((`testBit` 2) <$> btn)
    toggle = detectRisingEdge ((`testBit` 1) <$> btn)

    sel =
      cycleSelect
        toggle
        Bounce
        ( \case
            Clicky -> Counter
            Counter -> Bounce
            Bounce -> Clicky
        )

    counter = regEn 0 (slowClk d20) (counter + 1)

    clicky :: Signal dom (BitVector 8) = bitCoerce <$> mooreB clickyNext id (0 :: Unsigned 8) (up, down, reset)

    (bounceDir, bounce) =
      unbundle $ regEn (R, 0b1000_0000) (slowClk d20) (bounceNext @8 <$> bounceDir <*> bounce)

clickyNext ::
  (KnownNat n) =>
  "current" ::: Unsigned n ->
  ("up" ::: Bool, "down" ::: Bool, "reset" ::: Bool) ->
  "next" ::: Unsigned n
clickyNext _current (_, _, True) = 0
clickyNext current (True, _, _) = current + 1
clickyNext current (_, True, _) = current - 1
clickyNext current (False, False, _) = current

data Direction = L | R
  deriving (Generic, NFDataX)

bounceNext :: (KnownNat n) => Direction -> BitVector n -> (Direction, BitVector n)
bounceNext R bv
  | bitToBool (lsb bv) = (L, bv)
  | otherwise = (R, bv .>>. 1)
bounceNext L bv
  | bitToBool (msb bv) = (R, bv)
  | otherwise = (L, bv .<<. 1)

-- detectRisingEdge :: KnownDomain dom => Signal dom Bool -> Signal dom Bool
detectRisingEdge :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Bool
detectRisingEdge signal = do
  lastVal <- delay False signal
  currentVal <- signal
  pure (not lastVal && currentVal)

cycleSelect :: (NFDataX a, HiddenClockResetEnable dom) => Signal dom Bool -> a -> (a -> a) -> Signal dom a
cycleSelect toggle initial next =
  let s = regEn initial toggle (next <$> s)
   in s
