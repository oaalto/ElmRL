module Display.Model exposing (..)

import Css exposing (Color)
import Css.Colors exposing (gray, black)
import Array exposing (Array)


type alias Model =
    { width : Int
    , height : Int
    , layers : Array Layer
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias Rectangle =
    { topLeft : Point
    , bottomRight : Point
    }


type alias Layer =
    { bounds : Rectangle
    , tiles : Array Tile
    }


type alias Tile =
    { color : Color
    , c : Char
    }


type alias Size =
    { width : Int
    , height : Int
    }


initialModel : Int -> Int -> Model
initialModel width height =
    Model width height (Array.fromList [ initialLayer width height ])


initialLayer : Int -> Int -> Layer
initialLayer width height =
    Layer initialBounds
        (initialTiles width height)


initialBounds : Rectangle
initialBounds =
    Rectangle (Point 0 0) (Point 80 25)


initialTiles : Int -> Int -> Array Tile
initialTiles width height =
    Array.repeat (width * height) initialTile


initialTile : Tile
initialTile =
    Tile gray '#'


emptyTile : Tile
emptyTile =
    Tile black ' '


emptyTiles : Int -> Int -> Array Tile
emptyTiles width height =
    Array.repeat (width * height) emptyTile


emptyLayer : Size -> Layer
emptyLayer { width, height } =
    Layer (getBounds width height) (emptyTiles width height)


getBounds : Int -> Int -> Rectangle
getBounds width height =
    Rectangle (Point 0 0) (Point width height)


getSize : Rectangle -> Size
getSize rectangle =
    Size (rectangle.bottomRight.x - rectangle.topLeft.x) (rectangle.bottomRight.y - rectangle.topLeft.y)
