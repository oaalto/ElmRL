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
                Array.foldl combineLayer (emptyLayer (sizeWidth layer.size) (sizeHeight layer.size)) layers

            Nothing ->
                Layer initialSize (drawString "Error")


combineLayer : Layer -> Layer -> Layer
combineLayer from to =
    let
        toLayerWidth =
            sizeWidth to.size

        fromLayerWidth =
            sizeWidth from.size

        fromLayerHeight =
            sizeHeight from.size

        topLeftX =
            from.size.topLeft.x

        topLeftY =
            from.size.topLeft.y

        targetRows =
            Debug.log "indices" (List.range topLeftY (topLeftY + fromLayerHeight - 1))

        allIndices =
            Debug.log "all indices" (List.concat (List.indexedMap (calculateIndices topLeftX fromLayerWidth toLayerWidth) targetRows))
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
        lines =
            List.range 0 (sizeHeight layer.size - 1)
    in
        List.concatMap (renderLine layer) lines


renderLine : Layer -> Int -> List (Html msg)
renderLine layer index =
    let
        start =
            index * (sizeWidth layer.size)

        end =
            start + (sizeWidth layer.size)

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
