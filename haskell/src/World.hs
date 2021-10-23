module World where

import Graphics.Gloss

-- |The size of an individual
data Size = Default | Small | Big
  deriving(Show, Eq)

-- |Possible positions for an object in the frame.
data Position = Center | XY Int Int
  deriving(Show, Eq)

-- |An attribute that an individual might possess, such as color or size
data Attribute = AColor Color | Size Size
  deriving(Show, Eq)

-- |Animal is an enumeration over possible animals which can be created.
--  In the future, animals will be able to interact with each other.
data Animal = Cat
  deriving(Show, Eq)

-- |An animal with attributes and a position
data Individual = Individual [Attribute] Animal Position
  deriving(Show, Eq)

-- |A world contains individuals and a String field with user input.
data World = World [Individual] String
  deriving(Show, Eq)

-- | The empty world
defaultWorld = World [] ""

-- | The basic cat image. In the future, we might load a bitmap here.
baseCat = pictures [
        circle 40,
        line [(32, 24), (32, 36), (24, 32)],
        line [(-32, 24),  (-32, 36), (-24, 32)]
    ]

-- |Applies the attribute to the picture
applyAttribute :: Attribute -> Picture -> Picture
applyAttribute (AColor color) = Color color
applyAttribute (Size Small) = Scale 0.7 0.7
applyAttribute (Size Default) = Scale 1 1
applyAttribute (Size Big) = Scale 1.2 1.2

-- |Renders the individual, applying all attributes.
individualToPicture :: Individual -> Picture
individualToPicture (Individual attrs Cat _) = foldr applyAttribute baseCat attrs

-- |Renders the world, drawing all individuals into the frame.
-- If the world contains text, the text is also drawn to screen
worldToPicture :: World -> IO Picture
worldToPicture (World individuals s) = return (
        pictures (Scale 0.2 0.2 (Text s) : map individualToPicture individuals)
    )

-- |Steps the simulation. Currently does nothing.
--  In the future, this should simulate how animals interact.
updateWorld :: Float -> World -> IO World
updateWorld _ = return
