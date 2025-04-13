-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Top where

import Clash.Prelude
import Clash.Explicit.Prelude (noReset)

createDomain vSystem{vName="Dom25", vPeriod=hzToPeriod 25e6}

-- | @topEntity@ is Clash@s equivalent of @main@ in other programming languages.
-- Clash will look for it when compiling "Example.Project" and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a @topEntity@
-- must be monomorphic and must use non- recursive types. Or, to put it
-- hand-wavily, a @topEntity@ must be translatable to a static number of wires.
--
-- Top entities must be monomorphic, meaning we have to specify all type variables.
-- In this case, we are using the @Dom50@ domain, which we created with @createDomain@
-- and we are using 8-bit unsigned numbers.
topEntity
  :: "clk_25mhz" ::: Clock Dom25
  -> "led" ::: Signal Dom25 (BitVector 8)
topEntity clk = exposeClockResetEnable ledsOn clk noReset enableGen

-- -- To specify the names of the ports of our top entity, we create a @Synthesize@ annotation.
{-# ANN topEntity
  (Synthesize
    { t_name = "topEntity"
    , t_inputs = [ PortName "clk_25mhz" ]
    , t_output = PortName "led"
    }) #-}

-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}

ledsOn
  :: (HiddenClockResetEnable dom)
  => Signal dom (BitVector 8)
ledsOn = 0b10101010
