open System
open System.Threading
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

type Cell =
    | Empty
    | Full

type Rule = (Cell * Cell * Cell) -> Cell

   
let generateStandardFirstRow (width: int) =
    if width % 2 = 0 then
        invalidArg "width" (sprintf $"Value must be an odd number. Value passed was %d{width}." )

    Seq.init
        width
        (function 
        | n when n = (width / 2) -> Full
        | _ -> Empty)

let generateNextRow (rule: Rule) (row: Cell seq) =
    let generatedCells =
        row
        |> Seq.windowed 3
        |> Seq.map (fun v -> rule (v.[0], v.[1], v.[2]))

    seq {
        yield Empty
        yield! generatedCells
        yield Empty
    }

let generatePattern (rule: Rule) (firstRow: Cell seq) =
    firstRow
    |> Seq.unfold (fun row -> Some(row, (generateNextRow rule row)))


let drawCell =
    function
    | Full -> "X"
    | Empty -> "."

let drawGrid (grid: Cell seq seq) =
    grid
    |> Seq.map (fun row -> row |> Seq.map drawCell |> String.concat "")
    |> String.concat "\n"

let cols = 1001
let rows = 500

let white = new Rgba32(255F,255F,100F,1F)
let black = new Rgba32(0F,0F,0F,1F)
let withIndexes x = x |> Seq.mapi (fun index item -> (index, item))

let image = new Image<Rgba32>(cols,rows)

let drawImage (grid : Cell seq seq) =
    for(rowIndex, row) in withIndexes grid do
        for (cellIndex, cell) in withIndexes row do
            let colour =
                match cell with
                | Full -> black
                | Empty -> white
            image.[cellIndex, rowIndex] <- colour

let generateRule(ruleNumber: byte) =
    let ruleBitString = Convert.ToString(ruleNumber, 2).PadLeft(8,'0')
    let patternToBitStringIndex = function
        | (Full, Full, Full) -> 0
        | (Full, Full, Empty) -> 1
        | (Full, Empty, Full) -> 2
        | (Full, Empty, Empty) -> 3
        | (Empty, Full, Full) -> 4
        | (Empty, Full, Empty) -> 5
        | (Empty, Empty, Full) -> 6
        | (Empty, Empty, Empty) -> 7
    let ruleFn(cells: Cell * Cell * Cell) =
        let bitIndex = patternToBitStringIndex cells
        
        match ruleBitString.[bitIndex] with
        | '0' -> Empty
        | '1' -> Full
        | _ -> raise (Exception("Unexpected input"))
    ruleFn
    
[<EntryPoint>]
let main argv =
    let maximum = 254uy
    for id  in 0uy .. maximum do
        let grid =
            generateStandardFirstRow cols
            |> generatePattern (generateRule id)
            |> Seq.take rows
        drawImage grid
        let filename = sprintf "rule%0-d.png" id
        image.Save(filename)
        printfn "%0-d of %d" id maximum
        image = new Image<Rgba32>(cols, rows)
    0 // return an integer exit code
