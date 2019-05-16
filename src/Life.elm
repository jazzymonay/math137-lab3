module Life exposing (..)

import Debug exposing (..)

type alias Cell = { x : Int, y : Int }

type CellStatus = Dead | Alive

type alias Board = Cell -> CellStatus

-- calculates the next status of a cell, given
-- the number of living neighbors and the cell's
-- current status.
nextStatus : Int -> CellStatus -> CellStatus
nextStatus numberOfLivingNeighbors currentStatus =
  case currentStatus of
        Alive ->
            if numberOfLivingNeighbors < 2
                then Dead
                else if numberOfLivingNeighbors > 3
                then Dead
                else Alive
        Dead -> 
            if numberOfLivingNeighbors == 3
             then Alive 
             else Dead 

-- calculates the number of living neighbors of a cell,
-- given a board and a cell.
livingNeighbors : Board -> Cell -> Int
livingNeighbors currentBoard { x, y } = 
    let
        allNeighbors : List Cell
        allNeighbors = [ { x = x - 1, y = y + 1 },
                   { x = x, y = y + 1 },
                   { x = x + 1, y = y + 1 },
                   { x = x - 1, y = y },
                   { x = x + 1, y = y },
                   { x = x - 1, y = y - 1 },
                   { x = x, y = y - 1 },
                   { x = x + 1, y = y - 1}
                         ]

        allStatuses : List CellStatus
        allStatuses = List.map currentBoard allNeighbors 

        numericalStatus : List Int
        numericalStatus = List.map statusToInt allStatuses 

        statusToInt : CellStatus -> Int 
        statusToInt j = 
            case j of 
                Alive -> 1
                Dead -> 0

        sumOfList : Int 
        sumOfList = List.sum numericalStatus 
    
    in

    sumOfList
  

-- calculates the next board given the current board.
nextBoard : Board -> Board
nextBoard currentBoard =
  let
        newBoard : Board
        newBoard (cell) =
            let
                currStat = currentBoard (cell)
                currLivingNbrs = livingNeighbors (currentBoard) (cell)
                newStatus = nextStatus (currLivingNbrs) (currStat)
            in
            newStatus

       
    in
    newBoard
