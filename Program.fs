open System

type Player = X | O
type Cell = Empty | Occupied of Player

type Board = Cell [,]

let createBoard () =
    Array2D.init 3 3 (fun _ _ -> Empty)

let printBoard (board: Board) =
    printfn "-------------"
    for i in 0 .. 2 do
        printf "| "
        for j in 0 .. 2 do
            match board.[i, j] with
            | Empty -> printf "  | "
            | Occupied player -> printf "%A | " player
        printfn "\n-------------"

let isBoardFull (board: Board) =
    let mutable full = true
    for i in 0 .. 2 do
        for j in 0 .. 2 do
            if board.[i, j] = Empty then
                full <- false
    full

let isWinningMove (player: Player) (board: Board) =
    let checkRow row =
        let mutable win = true
        for i in 0 .. 2 do
            if board.[row, i] <> Occupied player then
                win <- false
        win

    let checkColumn col =
        let mutable win = true
        for i in 0 .. 2 do
            if board.[i, col] <> Occupied player then
                win <- false
        win

    let checkDiagonal =
        (board.[0, 0] = Occupied player && board.[1, 1] = Occupied player && board.[2, 2] = Occupied player) ||
        (board.[0, 2] = Occupied player && board.[1, 1] = Occupied player && board.[2, 0] = Occupied player)

    checkRow 0 || checkRow 1 || checkRow 2 ||
    checkColumn 0 || checkColumn 1 || checkColumn 2 ||
    checkDiagonal

let makeMove (player: Player) (row: int) (col: int) (board: Board) =
    if board.[row, col] = Empty then
        board.[row, col] <- Occupied player
        true
    else
        false

let askForRematch () =
    printfn "Do you want a rematch? (yes/no)"
    let input = Console.ReadLine()
    match input.ToLower() with
    | "yes" -> true
    | _ -> false

let rec playGame (player: Player) (board: Board) =
    printBoard board

    printfn "Player %A's turn" player
    printfn "Enter row and column (0-2) separated by space:"

    let input = Console.ReadLine()
    match input.Split(' ') with
    | [| rowStr; colStr |] ->
        match Int32.TryParse(rowStr), Int32.TryParse(colStr) with
        | (true, row), (true, col) ->
            if row >= 0 && row <= 2 && col >= 0 && col <= 2 then
                if makeMove player row col board then
                    if isWinningMove player board then
                        printfn "Player %A wins!" player
                        if askForRematch() then
                            playGame X (createBoard())
                    elif isBoardFull board then
                        printfn "It's a draw!"
                        if askForRematch() then
                            playGame X (createBoard())
                    else
                        let nextPlayer = match player with X -> O | O -> X
                        playGame nextPlayer board
                else
                    printfn "Invalid move. Try again."
                    playGame player board
            else
                printfn "Invalid input. Try again."
                playGame player board
        | _ ->
            printfn "Invalid input. Try again."
            playGame player board
    | _ ->
        printfn "Invalid input. Try again."
        playGame player board

[<EntryPoint>]
let main argv =
    let board = createBoard()
    playGame X board
    0