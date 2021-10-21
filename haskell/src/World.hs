module World where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data Size = Default
data Animal = Cat
data Position = Center | XY Int Int

data Attribute = AColor Color | Size Size

data Individual = Individual [Attribute] Animal Position

data World = World [Individual] String

defaultWorld :: World
defaultWorld = World [] ""

applyAttribute :: Attribute -> Picture -> Picture
applyAttribute (AColor color) = Color color
applyAttribute (Size Default) = Scale 1 1

worldToPicture :: World -> IO Picture
worldToPicture (World individuals s) = return (
        pictures (Scale 0.2 0.2 (Text s) : map individualToPicture individuals)
    )

updateWorld :: Float -> World -> IO World
updateWorld _ = return

drawCat :: Picture
drawCat = pictures [
        circle 40,
        line [(32, 24), (32, 36), (24, 32)],
        line [(-32, 24),  (-32, 36), (-24, 32)]
    ]

individualToPicture :: Individual -> Picture
individualToPicture (Individual attrs Cat _) = foldr applyAttribute drawCat attrs
