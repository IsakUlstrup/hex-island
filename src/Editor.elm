module Editor exposing (EditorState, Tool(..), init, setTool, toggle, toolToString)


type Tool
    = Raise Int
    | Lower Int
    | Level Int
    | Pan


toolToString : Tool -> String
toolToString tool =
    case tool of
        Raise _ ->
            "raise"

        Lower _ ->
            "lower"

        Level _ ->
            "level"

        Pan ->
            "pan"


type alias EditorState =
    { enabled : Bool
    , selectedTool : Tool
    , brushRadius : Int
    }


init : EditorState
init =
    EditorState False (Raise 1) 2


toggle : EditorState -> EditorState
toggle editor =
    { editor | enabled = not editor.enabled }


setTool : Tool -> EditorState -> EditorState
setTool tool editor =
    { editor | selectedTool = tool }
