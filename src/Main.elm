module Main exposing (..)

import Html exposing (..)
import Array
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { pc : Int
    , ram : Array.Array Instruction
    , clkCycle : Int
    , tCycle : TCycle
    , mCycle : MCycle
    , registers : RegisterValues
    }


type Register
    = A
    | B
    | C
    | D
    | E
    | H
    | L


type TCycle
    = T1
    | T2
    | T3
    | T4


type MCycle
    = M1
    | M2
    | M3


type Instruction
    = NOP
    | HALT
    | LD Register Register
    | LDI Register Int
    | ADD Register
    | JRZ Int


type alias RegisterValues =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    , e : Int
    , h : Int
    , l : Int
    }


init : Model
init =
    { pc = 0
    , ram = initRam
    , clkCycle = 0
    , tCycle = T1
    , mCycle = M1
    , registers = RegisterValues 0 0 0 0 0 0 0
    }


initRam : Array.Array Instruction
initRam =
    Array.fromList
        [ LDI A 10
        , LDI B -1
        , ADD B
        , JRZ -1
        , HALT
        ]


type Msg
    = NoOp
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Tick _ ->
            { model
                | clkCycle = model.clkCycle + 1
                , tCycle = changeTCycleState model.tCycle
                , mCycle = changeMCycleState model.tCycle model.mCycle
            }
                ! []


changeTCycleState : TCycle -> TCycle
changeTCycleState tCycle =
    case tCycle of
        T1 ->
            T2

        T2 ->
            T3

        T3 ->
            T4

        T4 ->
            T1


changeMCycleState : TCycle -> MCycle -> MCycle
changeMCycleState tCycle mCycle =
    case tCycle of
        T4 ->
            case mCycle of
                M1 ->
                    M2

                M2 ->
                    M3

                M3 ->
                    M1

        _ ->
            mCycle


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "Z80 Emulator" ]
        , p [] [ text ("PC: " ++ (toString <| model.clkCycle)) ]
        , p [] [ text ("MCycle: " ++ (toString <| model.mCycle)) ]
        , p [] [ text ("TCycle: " ++ (toString <| model.tCycle)) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1 * Time.second) Tick
