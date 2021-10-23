module World where

import Graphics.Gloss

data Size = Default | Small | Big
  deriving(Show, Eq)

data Animal = Cat
  deriving(Show, Eq)

-- |Possible positions for an object in the frame.
data Position = Center | XY Int Int
  deriving(Show, Eq)

-- |An attribute that an individual might possess, such as color or size
data Attribute = AColor Color | Size Size
  deriving(Show, Eq)

-- |An animal with attributes and a position
data Individual = Individual [Attribute] Animal Position
  deriving(Show, Eq)

data World = World [Individual] String
  deriving(Show, Eq)

defaultWorld :: World
defaultWorld = World [] ""

-- |Applies the attribute to the picture
applyAttribute :: Attribute -> Picture -> Picture
applyAttribute (AColor color) = Color color
applyAttribute (Size Small) = Scale 0.7 0.7
applyAttribute (Size Default) = Scale 1 1
applyAttribute (Size Big) = Scale 1.2 1.2

-- |Renders the world
-- If it contains text, the text is also drawn to screen
worldToPicture :: World -> IO Picture
worldToPicture (World individuals s) = return (
        pictures (Scale 0.2 0.2 (Text s) : map individualToPicture individuals)
    )

updateWorld :: Float -> World -> IO World
updateWorld _ = return

baseCat = pictures [
        circle 40,
        line [(32, 24), (32, 36), (24, 32)],
        line [(-32, 24),  (-32, 36), (-24, 32)]
    ]

individualToPicture :: Individual -> Picture
individualToPicture (Individual attrs Cat _) = foldr applyAttribute baseCat attrs
