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
    { size : Rectangle
    , tiles : Array Tile
    }


type alias Tile =
    { color : Color
    , c : Char
    }


initialModel : Int -> Int -> Model
initialModel width height =
    Model width height (Array.fromList [ initialLayer width height ])


initialLayer : Int -> Int -> Layer
initialLayer width height =
    Layer initialSize (initialTiles width height)


initialSize : Rectangle
initialSize =
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


emptyLayer : Int -> Int -> Layer
emptyLayer width height =
    Layer (getSize width height) (emptyTiles width height)


getSize : Int -> Int -> Rectangle
getSize width height =
    Rectangle (Point 0 0) (Point width height)


sizeHeight : Rectangle -> Int
sizeHeight rectangle =
    rectangle.bottomRight.y - rectangle.topLeft.y


sizeWidth : Rectangle -> Int
sizeWidth rectangle =
    rectangle.bottomRight.x - rectangle.topLeft.x
