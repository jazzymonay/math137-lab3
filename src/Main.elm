import Lib.Color as Color
import Lib.Graphics as Graphics
import Lib.Game as Game
import Lib.Keyboard as Keyboard
import Lib.Memoize as Memoize
import Lib.List as List

import Debug exposing (..)

import Life exposing (Board, Cell, CellStatus(..), nextBoard)


-- To define a program, we need
--   1. A type for the possible states         type State
--   2. A type for the possible events         type Event
--   3. A transition function                  update : event -> state -> state
--   4. An initial state                       init : state
--   5. A view function                        view : state -> Canvas event


-- A type for your game state. As your game gets more features, you will
-- probably add more fields to this type.
type alias State = { board : Board, paused : Bool }


-- A type for your game events. As your game gets more features, you will
-- probably add more variants to this type.
type Event = NoOp | CellClick Cell


main : Game.Game State Event
main =
  Game.game
    { init = initialState
    , update = updateGame
    , view = drawGame
    }


-- This is the board our game will start with.
initialState : State
initialState =
  let
    startingBoard : Board
    startingBoard cell = case ( cell.x, cell.y ) of
       (3,2) -> Alive
       (4,3) -> Alive
       (2,4) -> Alive
       (3,4) -> Alive
       (4,4) -> Alive 
       _ -> Dead
      
  in
  { board = startingBoard, paused = False }

memoStrat : Memoize.MemoizeStrategy (Int, Int) Cell CellStatus
memoStrat = 
  let 
      cellToPair cell = ( cell.x, cell.y )
      pairToCell (x0, y0) = { x = x0, y = y0}
      allPairs = List.map cellToPair allCells
      defaultStatus = Dead
  in
  { toKey = cellToPair, fromKey = pairToCell, domain = allPairs, default = defaultStatus }


-- This function uses the incoming event and the current game state to
-- decide what the next game state should be.
updateGame : Game.GameEvent Event -> State -> State
updateGame event currentState =
  case event of
    -- What to do when we get a `ClockTick`
    Game.ClockTick timestamp ->
      if currentState.paused
        then currentState
        else
          let
            updatedBoard = nextBoard (currentState.board)
            memoizedBoard = Memoize.memoize memoStrat updatedBoard
          in
          { board = memoizedBoard, paused = currentState.paused }

    -- What to do when the player presses or releases a key
    Game.Keyboard keyEvent ->
      case keyEvent of
        Keyboard.KeyEventDown Keyboard.KeySpace ->
          let
            newPaused : Bool 
            newPaused =
              case currentState.paused of
                True -> False 
                False -> True 
              
          in
          { board = currentState.board, paused = newPaused }
          -- todo "a new state with the same board but opposite paused"
        _ ->
          currentState

    -- What to do when we get a `NoOp`
    Game.Custom NoOp->
      currentState
    Game.Custom (CellClick cell) ->
      let
          newBoard = flipCell cell currentState.board
      in
      { board = newBoard, paused = currentState.paused }

flipCell : Cell -> Board -> Board
flipCell clickedCell oldBoard =
  let
      newBoard : Cell -> CellStatus
      newBoard cell =
        if clickedCell == cell -- if cell is the clicked cell
          then case oldBoard cell of
            Alive->Dead 
            Dead->Alive -- opposite of what old board would do
          else oldBoard cell -- same as what old board would do
        -- todo "if cell is the clicked cell, then give back the opposite of what the old board would have"
        -- todo "if cell is not the clicked cell, then give back what the old board would have"
  in
  newBoard

-- Pick a size for the game board.
-- Hint: Use this when you go to write `drawCell` and `drawGame`
boardSize : Int
boardSize = 50


-- The list of all cells based on your `boardSize`.
allCells : List Cell
allCells =
  let
    range = List.range 0 boardSize
    toCell (x_coord, y_coord) = { x = x_coord, y = y_coord }
  in
  List.map toCell (List.cross range range)


-- This function will use the game state to decide what to draw on the screen.
drawGame : State -> Graphics.Canvas Event
drawGame state =
  let
    drawCell : Cell -> Graphics.Svg Event 
    drawCell cell =
      let
        cellStatus : CellStatus
        cellStatus = state.board(cell)

        cellColor : Color.Color
        cellColor = case cellStatus of 
          Alive -> Color.blue
          Dead -> Color.fuchsia

      in
      Graphics.drawRect ({
        x0 = toFloat cell.x,
        y0 = toFloat cell.y,
        width = 5,
        height = 5,
        fill = cellColor,
        onClick = Just (CellClick cell)
      })

       

    cells : List (Graphics.Svg Event)
    cells = List.map drawCell allCells
        -- oneBigOleRect : Graphics.Svg Event
        -- oneBigOleRect =
        --   Graphics.drawRect 
        --     { x0 = todo "x0"
        --     , y0 = todo "y0"
        --     , width = todo "width"
        --     , height = todo "height"
        --     , fill = Color.black
        --     , onClick = Nothing 
        --     }
    
  in
  Graphics.canvas 
    { title = "conwaysLife"
    , widthPx = 300
    , heightPx = 300
    , xMax = 60
    , yMax = 60
    , children = cells
    } 
