module Display.View exposing (view)

import Display.Model exposing (..)
import Css exposing (..)
import Css.Colors exposing (black, gray)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Array exposing (Array)
import List.Extra as List


drawString : String -> Array Tile
drawString str =
    String.foldl strToTile Array.empty str


strToTile : Char -> Array Tile -> Array Tile
strToTile c tiles =
    Array.push (Tile gray c) tiles



{- We assume that the first layer is the largest. -}


flattenLayers : Array Layer -> Layer
flattenLayers layers =
    let
        firstLayer =
            Array.get 0 layers
    in
        case firstLayer of
            Just layer ->
                let
                    layerSize =
                        getSize layer.bounds
                in
                    Array.foldl combineLayer (emptyLayer layerSize) layers

            Nothing ->
                Layer initialBounds (drawString "Error")


combineLayer : Layer -> Layer -> Layer
combineLayer from to =
    let
        toLayerSize =
            getSize to.bounds

        fromLayerSize =
            getSize from.bounds

        topLeftX =
            from.bounds.topLeft.x

        topLeftY =
            from.bounds.topLeft.y

        targetRows =
            List.range topLeftY (topLeftY + fromLayerSize.height - 1)

        allIndices =
            List.concat (List.indexedMap (calculateIndices topLeftX fromLayerSize.width toLayerSize.width) targetRows)
    in
        List.foldl (replaceTile from) to allIndices


replaceTile : Layer -> ( Int, Int ) -> Layer -> Layer
replaceTile from ( targetIndex, sourceIndex ) to =
    let
        maybeTile =
            Array.get sourceIndex from.tiles
    in
        case maybeTile of
            Just tile ->
                let
                    tiles =
                        Array.set targetIndex tile to.tiles
                in
                    { to | tiles = tiles }

            Nothing ->
                to


calculateIndices : Int -> Int -> Int -> Int -> Int -> List ( Int, Int )
calculateIndices x fromWidth toWidth index i =
    let
        targetStart =
            i * toWidth + x

        targetEnd =
            targetStart + toWidth - 1

        targetIndices =
            List.range targetStart targetEnd

        sourceStart =
            index * fromWidth

        sourceEnd =
            sourceStart + fromWidth - 1

        sourceIndices =
            List.range sourceStart sourceEnd
    in
        List.zip targetIndices sourceIndices


view : Model -> Html msg
view model =
    let
        layer =
            flattenLayers model.layers
    in
        div [ css [ backgroundColor black, lineHeight (pct 80), display inlineBlock ] ]
            (renderLayer layer)


renderLayer : Layer -> List (Html msg)
renderLayer layer =
    let
        size =
            getSize layer.bounds

        lines =
            List.range 0 (size.height - 1)
    in
        List.concatMap (renderLine layer) lines


renderLine : Layer -> Int -> List (Html msg)
renderLine layer index =
    let
        size =
            getSize layer.bounds

        start =
            index * size.width

        end =
            start + size.width

        row =
            renderRow (Array.slice start end layer.tiles)
    in
        List.append row [ br [] [] ]


renderRow : Array Tile -> List (Html msg)
renderRow tiles =
    Array.toList (Array.map tileToSpan tiles)


tileToSpan : Tile -> Html msg
tileToSpan tile =
    span [ css [ fontFamily monospace, color tile.color ] ]
        [ text (String.fromChar tile.c) ]
