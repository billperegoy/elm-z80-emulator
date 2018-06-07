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
    , addrReg : Int
    , instrReg : Instruction
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
    | ILLEGAL
    | HALT
    | LD Register Register
    | LDI Register
    | ADD Register
    | JRNZ
    | DATA Int


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
    , addrReg = 0
    , instrReg = NOP
    , ram = initRam
    , clkCycle = 0
    , tCycle = T1
    , mCycle = M1
    , registers = RegisterValues 0 0 0 0 0 0 0
    }


initRam : Array.Array Instruction
initRam =
    Array.fromList
        [ LDI A
        , DATA 10
        , LDI B
        , DATA -1
        , ADD B
        , JRNZ
        , DATA -1
        , HALT
        ]


type Msg
    = Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            { model
                | clkCycle = model.clkCycle + 1
                , tCycle = changeTCycleState model.tCycle
                , mCycle = changeMCycleState model.tCycle model.mCycle model.instrReg
                , pc =
                    case ( model.mCycle, model.tCycle ) of
                        ( M1, T2 ) ->
                            model.pc + 1

                        _ ->
                            model.pc
                , addrReg =
                    case ( model.mCycle, model.tCycle ) of
                        ( M1, T1 ) ->
                            model.pc

                        -- This is the case of instructions which are 2 bytes
                        ( M2, T1 ) ->
                            case model.instrReg of
                                LDI _ ->
                                    model.pc

                                _ ->
                                    model.addrReg

                        _ ->
                            model.addrReg
                , instrReg =
                    case ( model.mCycle, model.tCycle ) of
                        ( M1, T2 ) ->
                            Array.get model.addrReg model.ram
                                |> Maybe.withDefault ILLEGAL

                        _ ->
                            model.instrReg
                , registers =
                    case ( model.mCycle, model.tCycle ) of
                        -- This is the cycle where we can execute immediate instructions
                        -- But I have a problem. I don't have rhe actual instruction opcode
                        -- saved. I only have the fetched immediate data.
                        ( M2, T3 ) ->
                            model.registers

                        _ ->
                            model.registers
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


changeMCycleState : TCycle -> MCycle -> Instruction -> MCycle
changeMCycleState tCycle mCycle instruction =
    case tCycle of
        T4 ->
            case mCycle of
                M1 ->
                    M2

                M2 ->
                    if twoCycleInstruction instruction then
                        M1
                    else
                        M3

                M3 ->
                    M1

        _ ->
            mCycle


twoCycleInstruction : Instruction -> Bool
twoCycleInstruction instruction =
    case instruction of
        LDI _ ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "Z80 Emulator" ]
        , p [] [ text ("Cycle: " ++ (toString <| model.clkCycle)) ]
        , p [] [ text ("MCycle: " ++ (toString <| model.mCycle)) ]
        , p [] [ text ("TCycle: " ++ (toString <| model.tCycle)) ]
        , p [] [ text ("PC: " ++ (toString <| model.pc)) ]
        , p [] [ text ("Address: " ++ (toString <| model.addrReg)) ]
        , p [] [ text ("Instruction: " ++ (toString <| model.instrReg)) ]
        , p [] [ text ("A: " ++ (toString <| model.registers.a)) ]
        , p [] [ text ("B: " ++ (toString <| model.registers.b)) ]
        , p [] [ text ("C: " ++ (toString <| model.registers.c)) ]
        , p [] [ text ("D: " ++ (toString <| model.registers.d)) ]
        , p [] [ text ("E: " ++ (toString <| model.registers.e)) ]
        , p [] [ text ("H: " ++ (toString <| model.registers.h)) ]
        , p [] [ text ("L: " ++ (toString <| model.registers.l)) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1 * Time.second) Tick
