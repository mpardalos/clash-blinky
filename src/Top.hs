{-# LANGUAGE ApplicativeDo #-}
-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Top where

import Clash.Explicit.Prelude (noReset)
import Clash.Num.Wrapping (Wrapping)
import Clash.Prelude
import Data.Functor ((<&>))

createDomain vSystem {vName = "Dom25", vPeriod = hzToPeriod 25e6}

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
  "btn" ::: Signal Dom25 (BitVector 7) ->
  "led" ::: Signal Dom25 (BitVector 8)
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

blinky ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector 7) ->
  Signal dom (BitVector 8)
blinky btn = mux select clicky slowLed
  where
    toggle = detectRisingEdge ((`testBit` 1) <$> btn)
    select = moore xor id False toggle

    slowLed =
      let counter = register (0 :: Unsigned 32) (counter + 1)
       in slice d28 d21 <$> counter

    up = detectRisingEdge ((`testBit` 3) <$> btn)
    down = detectRisingEdge ((`testBit` 4) <$> btn)
    reset = detectRisingEdge ((`testBit` 2) <$> btn)

    clicky = bitCoerce <$> mooreB clickyNext id (0 :: Unsigned 8) (up, down, reset)

clickyNext ::
  (KnownNat n) =>
  "current" ::: Unsigned n ->
  ("up" ::: Bool, "down" ::: Bool, "reset" ::: Bool) ->
  "next" ::: Unsigned n
clickyNext _current (_, _, True) = 0
clickyNext current (True, _, _) = current + 1
clickyNext current (_, True, _) = current - 1
clickyNext current (False, False, _) = current

-- detectRisingEdge :: KnownDomain dom => Signal dom Bool -> Signal dom Bool
detectRisingEdge :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Bool
detectRisingEdge signal = do
  lastVal <- delay False signal
  currentVal <- signal
  pure (not lastVal && currentVal)
