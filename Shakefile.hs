{-# LANGUAGE RecordWildCards, BlockArguments #-}
import Clash.Shake
import Clash.Shake.ECP5 (ecp5, fujprog)

import Development.Shake
import Development.Shake.FilePath
import Data.Traversable (for)
import Data.Foldable (for_)

outDir :: FilePath
outDir = "_build"

topHsModule :: String
topHsModule = "Top"

topModule :: String
topModule = "topEntity"

lpf :: String
lpf = "ulx3s_v20.lpf"

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } do
    useConfig "config.mk"

    phony "clean" do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    (clash, kit) <- clashRules (outDir </> "clash") Verilog
        [ "src" ]
        topHsModule
        [ "-Wno-partial-type-signatures"
        , "-fclash-intwidth=32" -- To play nicely with Spartan 3 and 6
        ] $
        return ()

    phony "clashi" $ clash ["--interactive", "src/Top.hs"]

    SynthKit{ bitfile, phonies } <- ecp5 "12k" kit
      (outDir </> "ulx3s" </> "synth") topModule (return [lpf])

    phony "bitstream" (need [bitfile])

    mapM_ (uncurry phony) phonies

    want [bitfile]
