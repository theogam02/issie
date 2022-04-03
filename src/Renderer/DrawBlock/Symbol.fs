(*
This module draws schematics component symbols. Each symbol is associated with a unique Issie component.
*)

module Symbol
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
open System.Text.RegularExpressions


/// --------- STATIC VARIABLES --------- ///
let GridSize = 30

type SymbolColour =
    static member Default = "lightgrey"
    static member Error = "red"
    static member Select = "lightgreen"

type SymbolOpacity =
    static member Default = 1.0
    static member DragAndDrop = 0.2
    static member Paste = 0.4

/// ---------- SYMBOL TYPES ---------- ///

type Symbol = {
    Pos: XYPos // Centre of Symbol
    InWidth0: int option // TODO: could combine into one int option * int option or (int*int) option
    InWidth1: int option
    Component: Component
    Colour: string
    Transform: Mat2x2
    PortOffsets: Map<PortId, XYPos> // Absolute port position relative to centre of symbol
    PortLabels : Map<PortId, string> option // Use default if None
    LabelBoundingBox: BoundingBox
    HighlightLabel: bool
    ShowInputPorts: bool
    ShowOutputPorts: bool
    Opacity: float
    Moving: bool
}

type SymbolDisp = | Sharp | Curved

type SymbolDispProps = {
    SymbolDisp: SymbolDisp
    Symbols: Symbol list
}

type Model = {
    Symbols: Map<ComponentId, Symbol>
    CopiedSymbols: Map<ComponentId, Symbol>
    Ports: Map<PortId, Port>
    CornerDisp: SymbolDisp
}

//----------------------------Message Type-----------------------------------//
type Msg =
    | MouseMsg of MouseT
    | AddSymbol of pos:XYPos * compType:ComponentType * lbl: string
    | CopySymbols of ComponentId list
    | DeleteSymbols of sIds:ComponentId list
    | ShowAllInputPorts | ShowAllOutputPorts | HideAllPorts 
    | MoveSymbols of compList: ComponentId list * move: XYPos
    | MoveLabel of compId: ComponentId * move: XYPos
    | MovePorts of compId: ComponentId * ports: PortId list * moveRight: bool * moveAmount: float
    /// Clockwise rotation by 90 degrees
    | RotateSymbol of ComponentId
    /// Vertical reflection
    | ReflectSymbol of ComponentId
    | ShowPorts of ComponentId list
    | SelectSymbols of ComponentId list // Issie interface
    | SymbolsHaveError of sIds: ComponentId list
    | ChangeLabel of sId : ComponentId * newLabel : string
    | PasteSymbols of sIds: ComponentId list
    | ColorSymbols of compList : ComponentId list * colour : HighLightColor
    | ErrorSymbols of errorIds: ComponentId list * selectIds: ComponentId list * isDragAndDrop: bool
    | ChangeNumberOfBits of compId:ComponentId * NewBits:int 
    | ChangeValue of compId: ComponentId * NewBits:int64 
    | ChangeConstant of compId: ComponentId * NewBits:int64 * NewText:string
    | ResetModel // For Issie Integration
    | LoadComponents of  Component list * LoadedComponent * LoadedComponent list // For Issie Integration
    | WriteMemoryLine of ComponentId * int64 * int64 // For Issie Integration 
    | WriteMemoryType of ComponentId * ComponentType
    | HighlightLabels of string
    | SymbolRenderCurved of SymbolDisp

//-----------------------------HELPER TYPES FOR COMPONENT DEPENDENT INFO----------------//

type private PortLabels = | PortLabels of Map<PortDirection, string list>

type private SymbolCoords = | SymbolCoords of Map<PortDirection, XYPos list>

/// Helper type for passing in component parameters when initialising components
type private CompInfo = {
    NumInPorts : int
    NumOutPorts : int 
    HGrids : int
    WGrids : int
}

/// Updates the SymbolInfo stored in sym's component based on the attributes in sym
let private updateSymbolInfo (sym: Symbol): Symbol =
    {sym with
        Component = {sym.Component with
                        SymbolInfo = Some {
                                            Transform = sym.Transform
                                            PortOffsets = sym.PortOffsets
                                            LabelBoundingBox = sym.LabelBoundingBox
                                            PortLabels = sym.PortLabels
                                        }
                    }
    }

//-----------------------------HELPER TYPES FOR DRAWING SYMBOLS ----------------//

// Type to hold extra shape coordinates (e.g. clock) and whether to use stroke colour
type private ExtraShape = {
    SPos : XYPos list list
    ColourStroke : bool
}

/// Helper type for passing in text parameters when adding text
type private TextInfo = {
    TPos : XYPos
    Text : string
    Anchor : string
    Weight : string
    Size : string
    Fill : string
}

//---------------------------------ARITHMETIC HELPERS-------------------------------//
/// Debug print
let pipeP (x: 'a) =
    printfn "%A" x
    x

/// Shift centre position towards top left position
let posToTopLeft (w : int) (h : int) (centrePos : XYPos) : XYPos =
    {
        X = centrePos.X - float w / 2.0
        Y = centrePos.Y - float h / 2.0
    }

/// Shift top left position towards centre position
let posToCentre (w : int) (h : int) (topLeftPos : XYPos) : XYPos =
    {
        X = topLeftPos.X + float w / 2.0
        Y = topLeftPos.Y + float h / 2.0
    }

//---------------------------------TRANSFORMATION HELPERS---------------------------//

/// Return transformed component coordinate
/// Note: `coord` has origin (0,0) at top-left of symbol
let private transformCoord (transform : Mat2x2) (w:int) (h:int) (coord : XYPos) : XYPos =
    coord
    |> posToTopLeft w h
    |> (*) transform
    |> posToCentre w h

/// Applies transformation to port direction
/// - First column of transform matrix is new direction of PRight
/// - Second column of transform matrix is new direction of PTop
/// - Special case for reflection
let transformPortDir (transform : Mat2x2) (portDir : PortDirection) : PortDirection =
    let vectDir (vect : XYPos) : PortDirection =
        match vect with
        | {X= 1.; Y= 0.} -> PRight
        | {X= -1.; Y= 0.} -> PLeft
        | {X= 0.; Y= 1.} -> PTop
        | {X= 0.; Y= -1.} -> PBottom
        | _ -> failwithf "Transform matrix is not valid"

    let flip = 
        match transform.X.Y <> 0 && transform.X.Y = transform.Y.X with
        | true -> -1.
        | false -> 1.

    let col1, col2 = {X=transform.X.X; Y=transform.Y.X}, {X=transform.X.Y; Y=transform.Y.Y}
    match portDir with
    | PRight -> col1 * flip
    | PLeft -> col1 * -flip
    | PTop -> col2 * flip
    | PBottom -> col2 * -flip
    |> vectDir


//---------------------------------TITLE HELPERS------------------------------------//

///Insert titles compatible with greater than 1 buswidth
let title name bitwidth = 
    match bitwidth = 1 with
    | true -> name
    | false -> name + "(" + string(bitwidth-1) + "..0)"

//-----------------------------GETTER HELPERS FOR COMPONENT DEPENDENT INFO----------------//

/// Get component labels (prefix of symbol title)
let getDefaultPrefix compType = 
    match compType with
    | Not | And | Or | Xor | Nand | Nor | Xnor -> "G"
    | Mux2 -> "MUX"
    | Demux2 -> "DM"
    | NbitsAdder _ -> "A"
    | NbitsNot _ -> "NOT"
    | NbitsAnd _ -> "AND"
    | NbitsOr _ -> "OR"
    | NbitsXor _ -> "XOR"
    | NbitsNand _ -> "NAND"
    | NbitsNor _ -> "NOR"
    | NbitsXnor _ -> "XNOR"
    | DFF | DFFE -> "FF"
    | Register _ | RegisterE _ -> "REG"
    | AsyncROM1 _ -> "AROM"
    | ROM1 _ -> "ROM"
    | RAM1 _ -> "RAM"
    | AsyncRAM1 _ -> "ARAM"
    | Custom c ->
        c.Name + (if c.Name |> Seq.last |> System.Char.IsDigit then "." else "")
    | Constant1 _ -> "C"
    | BusCompare _ -> "EQ"
    | Decode4 -> "DEC"
    | BusSelection _ -> "SEL"
    | _ -> ""

/// Get text to be put inside different Symbols depending on their ComponentType
let getName compType =
    match compType with
    | And | Nand-> "&"
    | Or | Nor-> "≥1"
    | Xor | Xnor -> "=1"
    | Not -> "1"
    | Decode4 -> "Decode"
    | NbitsAdder bits -> title "Adder" bits
    | Register bits | RegisterE bits -> title "Register" bits
    | AsyncROM1 _ -> "Async-ROM"
    | ROM1 _ -> "Sync-ROM"
    | RAM1 _ -> "Sync-RAM"
    | AsyncRAM1 _ -> "Async-RAM"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | NbitsNot bits -> title "N-bits-NOT" bits
    | NbitsAnd bits -> title "N-bits-And" bits
    | NbitsOr bits -> title "N-bits-Or" bits
    | NbitsXor bits -> title "N-bits-Xor" bits
    | NbitsNand bits -> title "N-bits-Nand" bits
    | NbitsNor bits -> title "N-bits-Nor" bits
    | NbitsXnor bits -> title "N-bits-Xnor" bits
    | Custom custom -> custom.Name
    | _ -> ""

/// Get port labels for each PortDirection of the component
let private getPortLabels compType : PortLabels =
    match compType with
    | Decode4 -> 
        [(PLeft, ["Data"]);
         (PBottom, ["SEL"]);
         (PRight, ["0"; "1"; "2" ; "3"])]
    | NbitsAdder _ -> 
        [(PLeft, ["A";"B"]);
         (PBottom, ["Cin"]);
         (PRight, ["Cout"; "Sum"])]
    | Register _ | DFF -> 
        [(PLeft, ["D"]);
         (PRight, ["Q"])]
    | RegisterE _ | DFFE -> 
        [(PLeft, ["D"]);
         (PRight, ["Q"]);
         (PTop, ["EN"])]
    | ROM1 _ |AsyncROM1 _ -> 
        [(PLeft, ["Addr"]);
         (PRight, ["Dout"])]
    | RAM1 _ | AsyncRAM1 _ -> 
        [(PLeft, ["Addr"; "Din"]);
         (PRight, ["Dout"]);
         (PTop, ["Wen"])]
    | Mux2 -> 
        [(PLeft, ["0"; "1"]);
         (PBottom, ["SEL"]);
         (PRight, ["OUT"])]
    | Demux2 -> 
        [(PLeft, ["IN"]);
         (PBottom, ["SEL"]);
         (PRight, ["0"; "1"])]
    | NbitsNot _ ->
        [(PLeft, ["In"]);
         (PRight, ["Out"])]
    | NbitsAnd _
    | NbitsOr _
    | NbitsXor _
    | NbitsNand _
    | NbitsNor _
    | NbitsXnor _ -> 
        [(PLeft, ["P"; "Q"]);
         (PRight, ["Out"])]
    | Custom x -> 
        [(PLeft, List.map fst x.InputLabels);
         (PRight, List.map fst x.OutputLabels)]
    |_ -> []
   // |Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
   // |Demux4 -> (["IN"; "SEL"],["0"; "1";"2"; "3";])
   // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
   // |Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
   // |_ -> ([],[])
   // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .
    |> Map.ofList
    |> PortLabels


/// Get coordinates describing each line segments of side of a symbol
/// Coords of line segments given in anti-clockwise order from top-left to easily extract polygon coordinates
let private getSymbolCoords compType (w: float) (h: float) : SymbolCoords =
    match compType with
    | Input _ -> 
        [(PLeft, [{X=0.; Y=0.}; {X=0.; Y=h}]); 
         (PBottom, [{X=0.; Y=h}; {X=w*0.66; Y=h}]); 
         (PRight, [{X=w*0.66; Y=h}; {X=w; Y=h*0.5}; {X=w*0.66; Y=0.}]);
         (PTop, [{X=w*0.66; Y=0.}; {X=0.; Y=0.}])]
    | Constant1 _ -> 
        [(PLeft, [{X=0.; Y=0.}; {X=0.; Y=h}]);
         (PBottom, [{X=0.; Y=h}; {X=w*0.5; Y=h*0.5}; {X=w-1.; Y=h*0.5}]); 
         (PRight, [{X=w; Y=h*0.5};{X=w; Y=h*0.5}]);
         (PTop, [{X=w-1.; Y=h*0.5}; {X=w*0.5; Y=h*0.5}; {X=0.; Y=0};])]
    | IOLabel -> 
        [(PLeft, [{X=0.; Y=h*0.5}; {X=0.; Y=h*0.5}]);
         (PBottom, [{X=1.; Y=h*0.5}; {X=w-1.; Y=h*0.5}]);
         (PRight, [{X=w; Y=h*0.5}; {X=w; Y=h*0.5}]);
         (PTop, [{X=w-1.; Y=h*0.5}; {X=1.; Y=h*0.5}])]
    | Output _ | Viewer _ -> 
        [(PLeft, [{X=w*0.2; Y=0.}; {X=0.; Y=h*0.5}; {X=w*0.2; Y=h}]); 
         (PBottom, [{X=w*0.2; Y=h}; {X=w; Y=h}]); 
         (PRight, [{X=w; Y=h}; {X=w; Y=0.}]);
         (PTop, [ {X=w; Y=0.}; {X=w*0.2; Y=0.}])]
    | Demux2 -> 
        [(PLeft, [{X=0.; Y=h*0.2}; {X=0.; Y=h*0.8}]); 
         (PBottom, [{X=0.; Y=h*0.8}; {X=w; Y=h}]); 
         (PRight, [{X=w; Y=h}; {X=w; Y=0.}]);
         (PTop, [{X=w; Y=0.}; {X=0.; Y=h*0.2}])]
    | Mux2 -> 
        [(PLeft, [{X=0.; Y=0.}; {X=0.; Y=h}]); 
         (PBottom, [{X=0.; Y=h}; {X=w; Y=h*0.8}]); 
         (PRight, [{X=w; Y=h*0.8}; {X=w; Y=h*0.2}]);
         (PTop, [{X=w; Y=h*0.2}; {X=0.; Y=0.}])]
    // // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
    // // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
    | BusSelection _ |BusCompare _ -> 
        [(PLeft, [{X=0.; Y=0.}; {X=0.; Y=h}]); 
         (PBottom, [{X=0.; Y=h}; {X=w*0.6; Y=h}; {X=w*0.8; Y=h*0.7}; {X=w; Y=h*0.7}]); 
         (PRight, [{X=w; Y=h*0.7}; {X=w; Y=h*0.3}]);
         (PTop, [{X=w; Y=h*0.3}; {X=w*0.8; Y=h*0.3}; {X=w*0.6; Y=0.}; {X=0.; Y=0.}])]
    | _ -> 
        [(PLeft, [{X=0.; Y=0.}; {X=0.; Y=h}]); 
         (PBottom, [{X=0.; Y=h}; {X=w; Y=h}]); 
         (PRight, [{X=w; Y=h}; {X=w; Y=0.}]);
         (PTop, [{X=w; Y=0.}; {X=0.; Y=0.}])]
    |> Map.ofList
    |> SymbolCoords

/// Combine coordinates of each side into one list for entire symbol (to draw Polygon)
let private getPolygonCoords (SymbolCoords symbolCoords) : XYPos list =
    [PLeft; PBottom; PRight; PTop]
    |> List.map (fun side -> 
        match Map.tryFind side symbolCoords with
        | None -> []
        | Some coords -> coords)
    |> List.collect (function
        | _::tl -> tl
        | _ -> [])

/// Get size parameters of each component for initialisation with CompInfo type
let private getCompInfo compType (transform : Mat2x2) : CompInfo =
    // Custom component constants
    let portFont = "12px normal"
    let titleFont = "16px bold"
    
    let title = getName compType
    let (PortLabels portLabels) = getPortLabels compType

    // Get port labels accounting for transformation
    let getPortLabels (transformedSide : PortDirection) : string list =
        let originalSide = 
            [PLeft; PRight; PTop; PBottom]
            |> List.tryFind (fun side -> transformedSide = transformPortDir transform side)
        
        match originalSide with
        | None -> failwithf "Port Transformation failed"
        | Some portDir ->
            match Map.tryFind portDir portLabels with
            | None -> []
            | Some labels -> labels
    
    // Get max length of label lists from portLabels in specific direction (accounting for transformation)
    let gridLength (horizontal: bool) =
        let side1, side2  = 
            match horizontal with
            | true -> (PLeft, PRight)
            | false -> (PTop, PBottom)
        let maxLabelLength (portDir: PortDirection) : float =
            match getPortLabels portDir with
            | [] -> 0.
            | labelLst ->
                labelLst
                |> List.map (fun lbl -> DrawHelpers.getTextWidthInPixels (lbl, portFont))
                |> List.max
        
        // Make sure there is enough space for longest horizontal label
        let textLength = 
            DrawHelpers.getTextWidthInPixels (title,titleFont) * 0.5
            |> (+) (float ( max (maxLabelLength side1) (maxLabelLength side2) ))
            |> (*) 2. 
        
        textLength / float GridSize
        |> ceil 
        |> int
        |> (+) 1
    
    let wgrids = gridLength true
    let hgrids = gridLength false

    match compType with
    | ROM _ | RAM _ | AsyncROM _ -> 
        failwithf "What? Legacy RAM component types should never occur"
    | And | Nand | Or | Nor | Xnor | Xor | MergeWires ->  
        {NumInPorts=2; NumOutPorts=1; HGrids=2; WGrids=2}
    | Not -> 
        {NumInPorts=1; NumOutPorts=1; HGrids=2; WGrids=2}
    | Input _ | Constant1 _ | Constant _ -> 
        {NumInPorts=0; NumOutPorts=1; HGrids=1; WGrids=2}              
    | Output _ -> 
        {NumInPorts=1; NumOutPorts=0; HGrids=1; WGrids=2}
    | Viewer _ -> 
        {NumInPorts=1; NumOutPorts=0; HGrids=1; WGrids=1}
    | IOLabel ->
        {NumInPorts=1; NumOutPorts=1; HGrids=1; WGrids=1}
    | BusSelection _ | BusCompare _  -> 
        {NumInPorts=1; NumOutPorts=1; HGrids=1; WGrids=2}
    | Decode4 ->
        {NumInPorts=2; NumOutPorts=4; HGrids=4; WGrids=4}
    | SplitWire _ ->
        {NumInPorts=1; NumOutPorts=2; HGrids=2; WGrids=2}
    | Mux2 -> 
        {NumInPorts=3; NumOutPorts=1; HGrids=3; WGrids=2}
    | Demux2 ->
        {NumInPorts=2; NumOutPorts=2; HGrids=3; WGrids=2}
    | DFF -> 
        {NumInPorts=1; NumOutPorts=1; HGrids=3; WGrids=3}
    | DFFE -> 
        {NumInPorts=2; NumOutPorts=1; HGrids=3; WGrids=3}
    | Register _ | AsyncROM1 _ | ROM1 _ | NbitsNot _-> 
        {NumInPorts=1; NumOutPorts=1; HGrids=hgrids; WGrids=wgrids}
    | RegisterE _
    | NbitsAnd _
    | NbitsOr _
    | NbitsXor _
    | NbitsNand _
    | NbitsNor _
    | NbitsXnor _ ->
        {NumInPorts=2; NumOutPorts=1; HGrids=hgrids; WGrids=wgrids}
    | RAM1 _ | AsyncRAM1 _ -> 
        {NumInPorts=3; NumOutPorts=1; HGrids=hgrids; WGrids=wgrids}
    | NbitsAdder _ -> 
        {NumInPorts=3; NumOutPorts=2; HGrids=hgrids; WGrids=wgrids}
    // EXTENSION:    | Mux4 -> {NumInPorts=5; NumOutPorts=1; HGrids=5; WGrids=2}
    // EXTENSION:    | Mux8 -> {NumInPorts=9; NumOutPorts=1; HGrids=7; WGrids=2}
    // EXTENSION:    | Demux4 -> {NumInPorts=2; NumOutPorts=4; HGrids=150; WGrids=50}
    // EXTENSION:    | Demux8 -> {NumInPorts=2; NumOutPorts=8; HGrids=200; WGrids=50}
    | Custom custom ->
        let numInPorts = List.length custom.InputLabels
        let numOutPorts = List.length custom.OutputLabels
        {
            NumInPorts = numInPorts
            NumOutPorts = numOutPorts
            HGrids = max wgrids (1 + max numInPorts numOutPorts)
            WGrids = wgrids   
        }


/// ---------------------------GETTER HELPERS FOR DRAWING SYMBOLS ------------------------------------------------------ ///

/// Get coordinates of extra shapes to draw on symbol 
/// e.g. invertor or clock symbols
let private getExtraShapes (comp : Component) (w:float) (h:float) : ExtraShape =
    let clock = {SPos = [[{X=0.; Y=h-15.}; {X=8.; Y=h-21.}; {X=0.; Y=h-27.}]]; ColourStroke = false}
    // let clock = {SPos = [[{X=0.; Y=h-9.}; {X=8.; Y=h-15.}; {X=0.; Y=h-21.}]]; ColourStroke = false}
    let inverter = {SPos = [[{X=w; Y=h*0.5}; {X=w+9.; Y=h*0.5}; {X=w; Y=h*0.5-8.}]]; ColourStroke = false}
    match comp.Type with
        | Constant1 _ -> 
            {SPos = [[{X=w*0.5; Y=h*0.5}; {X=w; Y=h*0.5}]]; ColourStroke = false}
        | Nand | Nor | Xnor |Not -> inverter
        | MergeWires ->
            {SPos = [
                        [{X=0.; Y=h/3.}; {X=w*0.5; Y=h/3.}];
                        [{X=0.; Y=h*2./3.}; {X=w*0.5; Y=h*2./3.}];
                        [{X=w*0.5; Y=h/3.}; {X=w*0.5; Y=h*2./3.}];
                        [{X=w*0.5; Y=h*0.5}; {X=w; Y=h*0.5}]
                    ];
            ColourStroke = true}
        | SplitWire _ -> 
            {SPos = [
                        [{X=w*0.5; Y=h/3.}; {X=w; Y=h/3.}];
                        [{X=w*0.5; Y=h*2./3.}; {X=w; Y=h*2./3.}];
                        [{X=w*0.5; Y=h/3.}; {X=w*0.5; Y=h*2./3.}];
                        [{X=0.; Y=h*0.5}; {X=w*0.5; Y=h*0.5}]
                    ];
            ColourStroke = true}
        | Custom _ when comp.IsClocked -> clock
        | DFF | DFFE | Register _ |RegisterE _ | ROM1 _ |RAM1 _ | AsyncRAM1 _ -> clock
        | _ -> {SPos = []; ColourStroke = false}

/// Get extra text information to draw on symbol e.g. "clk" for synchronous components or bitwidths for MergeWires/SplitWire
let private getExtraText (sym : Symbol) (w:float) (h:float) (inWidth : int option * int option) : TextInfo list =
    let comp = sym.Component
    
    let bitwidthText msb lsb =
        match msb = lsb, msb >= lsb with
        | _, false -> ""
        | true, _ -> sprintf $"({msb})"
        | false, _ -> sprintf $"({msb}:{lsb})"
    
    let clkTxt = 
        [{
            TPos = {X=15.; Y=h-21.}
            Text = "clk"
            Anchor = "middle"
            Weight = "normal"
            Size = "12px"
            Fill = "black"
        }]
    let titles =
        [
            {
                TPos = {X=w*0.5; Y = h*0.5}
                Text = getName comp.Type
                Anchor = "middle"
                Weight = "bold"
                Size = "14px"
                Fill = "black"
            }
        ]
    
    let txtAnchor =
        match transformPortDir sym.Transform PBottom with
        | PLeft | PTop -> "end"
        | PBottom | PRight -> "start"

    match comp.Type with
    | Constant1 (_,_,txt) ->
        [{
            TPos = {X=w; Y=h} // txtPos txt "12px normal" // {X=w*0.5-5.; Y=h/2.-18.}
            Text = txt
            Anchor = txtAnchor
            Weight = "normal"
            Size = "12px"
            Fill = "black"
        }]
    | MergeWires -> 
        let hi, lo = 
            match inWidth with 
            | Some n, Some m  -> n, m
            | _ -> -1,-1
        [
            {
                TPos = {X=w*0.25; Y=h/3.-11.}
                Text = bitwidthText (lo-1) 0
                Anchor = "middle"
                Weight = "bold"
                Size = "9px"
                Fill = "black"
            };
            {
                TPos = {X=w*0.25; Y=h*2./3.-11.}
                Text = bitwidthText (hi+lo-1) lo
                Anchor = "middle"
                Weight = "bold"
                Size = "9px"
                Fill = "black"
            };
            {
                TPos = {X=w*0.75; Y=h*0.5-11.}
                Text = bitwidthText (hi+lo-1) 0
                Anchor = "middle"
                Weight = "bold"
                Size = "9px"
                Fill = "black"
            };
        ]
    | SplitWire mid -> 
        let msb, mid' = 
            match fst inWidth with 
            | Some bitwidth -> bitwidth-1, mid 
            | _ -> -1,0
        [
            {
                TPos = {X=w*0.75; Y=h/3.-11.}
                Text = bitwidthText (mid'-1) 0
                Anchor = "middle"
                Weight = "bold"
                Size = "9px"
                Fill = "black"
            };
            {
                TPos = {X=w*0.75; Y=h*2./3.-11.}
                Text = bitwidthText msb mid'
                Anchor = "middle"
                Weight = "bold"
                Size = "9px"
                Fill = "black"
            };
            {
                TPos={X=w*0.25; Y=h*0.5-11.}
                Text = bitwidthText msb 0
                Anchor = "middle"
                Weight = "bold"
                Size = "9px"
                Fill = "black"
            };
        ]
    | DFF |DFFE | Register _ |RegisterE _ | ROM1 _ |RAM1 _ | AsyncRAM1 _ -> clkTxt
    | Custom _ when comp.IsClocked -> clkTxt
    | BusSelection (bitwidth,lsb) -> 
        let txt =
            match bitwidth = 1 with 
            | true -> string(lsb)
            | false -> "(" + string(bitwidth + lsb - 1) + ".." + string(lsb) +  ")"
        [{
            TPos = {X=w; Y=h}
            Text = txt
            Anchor = txtAnchor
            Weight = "normal"
            Size = "12px"
            Fill = "black"
        }]
    | BusCompare (_,dec) ->
        let txt = "=" + NumberHelpers.hex(int dec)
        [{
            TPos = {X=w; Y=h}
            Text = txt
            Anchor = txtAnchor
            Weight = "bold"
            Size = "10px"
            Fill = "black"
        }]
    | Input bits | Output bits | Viewer bits ->
        let txt = title "" bits
        [{
            TPos = {X=w; Y=h}
            Text = txt
            Anchor = txtAnchor
            Weight = "normal"
            Size = "12px"
            Fill = "black"
        }]
    | _ -> []
    |> List.append titles

let private getLabelText sym compLabel (w:float) (h:float) : TextInfo =
    {
        TPos = {X = float sym.LabelBoundingBox.X - (sym.Pos.X - w/2.0) + sym.LabelBoundingBox.W/2.0
                Y = float sym.LabelBoundingBox.Y - (sym.Pos.Y - h/2.0) + sym.LabelBoundingBox.H/2.0 - 10.0}
        Text = compLabel
        Anchor = "middle"
        Weight = if sym.HighlightLabel then "bold" else "normal"
        Size = "16px"
        Fill = if sym.HighlightLabel then "blue" else "black"
    }

//-----------------------------------------PORT HELPER FUNCTIONS---------------------------------------------------

/// Calculates default position of ports with no rotation
let getDefaultPortPos (comp: Component) (port: Port) =
    // Add inputs from left to bottom to top - just as convention so all ports are covered
    // If no ports on bottom or top, just put all on left
    // Outputs added to right

    let ports =
        match port.PortType with
        | PortType.Input -> comp.InputPorts
        | PortType.Output -> comp.OutputPorts
    let portIdx = List.findIndex (fun (p:Port) -> p=port) ports
    
    // Get input side and index on that side
    let (PortLabels portLabels) = getPortLabels comp.Type
    let inputSides = [PLeft;PBottom;PTop]
    let accumSideLengths =
        (0, inputSides)
        ||> List.scan (fun acc portDir ->
            acc + List.length (match Map.tryFind portDir portLabels with | None -> [] | Some lst -> lst))
        |> List.map (fun acc -> acc - portIdx)

    let side, sideIdx =
        match port.PortType with
        | PortType.Output -> PRight, portIdx
        | PortType.Input ->
            match List.tryFindIndex (fun acc -> acc>0) accumSideLengths with
            | None -> PLeft, portIdx
            | Some sideIdx -> inputSides[sideIdx-1], accumSideLengths[sideIdx]-1

    let sideNumPorts =
        match Map.tryFind side portLabels with
        | None -> ports.Length // Put all ports on one side
        | Some labelList -> labelList.Length
    // Get coordinates of side
    let sideCoords =
        getSymbolCoords comp.Type comp.W comp.H 
        |> (fun (SymbolCoords symbolCoords) -> symbolCoords)
        |> Map.tryFind side
        |> function
        | None -> failwithf "Side doesn't exist for this symbol"
        | Some side -> side

    let getEqualSpacedPos (pos1 : XYPos) (pos2 : XYPos) =
        let startPos, endPos =
            match pos1.X <= pos2.X || pos1.Y <= pos2.Y with
            | true -> pos2, pos1
            | false -> pos1, pos2

        let w = endPos.X - startPos.X
        let h = endPos.Y - startPos.Y
        {
            X = startPos.X + w * (float (sideIdx+1) / float (sideNumPorts+1))
            Y = startPos.Y + h * (float (sideIdx+1) / float (sideNumPorts+1))
        }

    match sideCoords.Length = 2 with
    | false -> sideCoords[1] // TODO - get equally spaced points on multiple line segments
    | true -> getEqualSpacedPos sideCoords[0] sideCoords[1]
    |> posToTopLeft comp.W comp.H

/// Return port direction from port offset from centre
let getOrientation sym (portOffset : XYPos) : PortDirection =
        let comp = sym.Component
        let portPos = posToCentre comp.W comp.H portOffset

        let (SymbolCoords defaultCoords) = getSymbolCoords comp.Type comp.W comp.H 
        let transformCoords =
            (Map.empty, defaultCoords)
            ||> Map.fold (fun mp portDir coords -> 
                let newPortDir = transformPortDir sym.Transform portDir
                let newCoords = List.map (transformCoord sym.Transform comp.W comp.H) coords
                Map.add newPortDir newCoords mp
            )

        let coordOnLine ((l1,l2) : XYPos * XYPos) : bool =
            let inRange1D bound1 bound2 point : bool=
                match bound1,bound2 with
                | _ when bound1 <= bound2 ->
                    bound1 <= point && point <= bound2
                | _ ->
                    bound2 <= point && point <= bound1
            (l1.X - l2.X) * (portPos.Y - l1.Y) = (l1.Y - l2.Y) * (portPos.X - l1.X)
            && inRange1D l1.X l2.X portPos.X
            && inRange1D l1.Y l2.Y portPos.Y

        let onSide side : bool =
            Map.find side transformCoords
            |> List.pairwise 
            |> List.tryFind coordOnLine
            |> function
            | None -> false
            | Some _ -> true
        
        [PLeft;PBottom;PTop;PRight]
        |> List.tryFind onSide
        |> function
        | None -> PTop //failwithf "Couldn't find direction of port (not on any line segment of symbol)"
        | Some side -> side

/// Add symbol component's ports to Model.Ports map 
let addToPortModel (model: Model) (sym: Symbol) =
    let addOnePort (currentPorts: Map<PortId, Port>) (port: Port) =
        Map.add (PortId port.Id) port currentPorts
    
    (model.Ports, sym.Component.InputPorts @ sym.Component.OutputPorts) 
    ||> List.fold addOnePort

//------------------------------DRAWING HELPER FUNCTIONS ---------------------------------------------------//

let private createPolygon points colour opacity colourStroke = 
    let polygon =
        match colourStroke && (colour = SymbolColour.Error || colour = SymbolColour.Select) with
        | true -> {defaultPolygon with Fill = colour; Stroke = colour; FillOpacity = opacity}
        | false -> {defaultPolygon with Fill = colour; FillOpacity = opacity}
    [makePolygon points polygon]

let private createShape points colour opacity colourStroke = 
    let polygon =
        match colourStroke && (colour = SymbolColour.Error || colour = SymbolColour.Select) with
        | true -> {defaultPolygon with Fill = colour; Stroke = colour; FillOpacity = opacity}
        | false -> {defaultPolygon with Fill = colour; FillOpacity = opacity}
    [makeClosedPath points polygon]

/// Return text ReactElement with given parameters
let private addText textInfo : ReactElement list =
    let text =
        {defaultText with
            TextAnchor = textInfo.Anchor
            FontWeight = textInfo.Weight
            FontSize = textInfo.Size
            Fill = textInfo.Fill
        }
    [makeText textInfo.TPos.X textInfo.TPos.Y textInfo.Text text]

/// Position the port text on the side of the symbol given by portDirection
let private portText (portPos : XYPos) (txt : string) (portDir : PortDirection) (jump : float) : ReactElement list =
    let offset, txtAnchor =
        match portDir with
        | PLeft -> {X = 5.; Y = -7.}, "start"
        | PRight -> {X = -5.; Y = -7.}, "end"
        | PBottom -> {X = 0.; Y = -15. - jump}, "middle"
        | PTop -> {X = 0.; Y = 4. + jump}, "middle"
    let offsetPos = portPos + offset
    
    addText { TPos=offsetPos; Text=txt; Anchor=txtAnchor; Weight="normal"; Size="12px"; Fill="black" }

/// Draw port text at port locations given by offset (PortOffsets) from centre
let private drawPortsText (portList: Port List) (PortLabels portLabels) (sym: Symbol) : ReactElement list = 
    let comp = sym.Component

    let getPortText (portPos: XYPos) (portDir: PortDirection) (label: string) (idx: int): ReactElement list =
        let jump =
            match comp.Type with
            | Custom _ when idx % 2 = 1 -> 12. // Prevent label overlap
            | _ -> 0.
        portText (posToCentre comp.W comp.H portPos) label portDir jump

    match sym.PortLabels with
    | Some portLabels ->
        portLabels
        |> Map.toList
        |> List.mapi (fun idx (portId, label) ->
                let portPos = sym.PortOffsets[portId]
                let portDir = portPos |> getOrientation sym
                getPortText portPos portDir label idx
            )
        |> List.collect id
    | None ->
        let transformLabels =
            (Map.empty, portLabels)
            ||> Map.fold (fun mp portDir labels ->
                Map.add (transformPortDir sym.Transform portDir) labels mp
            )
        let assignLabelsToCoords portDir coords =
            match Map.tryFind portDir transformLabels with
            | None -> []
            | Some labels -> (List.zip coords labels) // Assign labels
            |> List.mapi (fun idx (portPos, label) -> getPortText portPos portDir label idx)
            |> List.collect id

        let portPos =
            portList
            |> List.map (fun port ->
                match Map.tryFind (PortId port.Id) sym.PortOffsets with
                | Some portOffset -> portOffset
                | _ -> failwithf "Can't find PortId in PortOffsets field of symbol")

        portPos
        |> List.groupBy (getOrientation sym)
        |> List.collect (fun (portDir, coords) -> assignLabelsToCoords portDir coords)

// Draw circles at port locations given by offset (PortOffsets) from centre
let private drawPorts (portList: Port List) (printPorts: bool) (sym: Symbol)= 
    match printPorts with
    | false -> []
    | true ->
        portList
        |> List.collect (fun port ->
            match Map.tryFind (PortId port.Id) sym.PortOffsets with
            | Some portOffset ->
                let pos = posToCentre sym.Component.W sym.Component.H portOffset
                [makeCircle pos.X pos.Y portCircle]
            | _ -> failwithf "Can't find portId in PortOffsets field of symbol")

// Draw small wires from ports to center of symbols. Fills a gap where curved symbols 
// have the edges pulled back from the port wire connection (unused as may not be needed)
let private drawPortWireExt (portList: Port List) (sym: Symbol) =
        portList
        |> List.collect (fun port ->
            match Map.tryFind (PortId port.Id) sym.PortOffsets with
            | Some portOffset ->
                let pos = posToCentre sym.Component.W sym.Component.H portOffset
                let posSym = posToCentre sym.Component.W sym.Component.H {X=0;Y=0}
                [makeLine posSym.X posSym.Y pos.X pos.Y defaultLine]
            | _ -> failwithf "Can't find portId in PortOffsets field of symbol")

//-----------------------MAIN INITIALISATION / DRAWING FUNCTIONS---------------------//

/// Initialise list of ports
let initPorts (numPorts : int) (hostId : string) (portType : PortType) : Port list =
    [0..(numPorts-1)]
    |> List.map (fun n ->
        {
            Id = JSHelpers.uuid ()
            PortNumber = Some n
            PortType = portType
            HostId = hostId
        }
    )
        
/// Initialise the specific type of component with its centre at the symbol position `pos`
let initComponent
    (pos: XYPos)
    (compType: ComponentType)
    (id: string)
    (label: string)
    (transform: Mat2x2)
    : Component =

    let param = getCompInfo compType transform
    let h = param.HGrids * GridSize
    let w = param.WGrids * GridSize
    let compPos = posToTopLeft w h pos
    {
        Id = id 
        Type = compType 
        Label = label 
        InputPorts = initPorts param.NumInPorts id PortType.Input 
        OutputPorts = initPorts param.NumOutPorts id PortType.Output 
        X = int compPos.X
        Y = int compPos.Y
        H = h
        W = w
        SymbolInfo = None
        IsClocked = false
    }

/// Returns new port offsets based on the component's default port positions
let getNewPortOffsets (comp: Component): Map<PortId, XYPos> =
    (Map.empty, comp.InputPorts @ comp.OutputPorts) 
    ||> List.fold (fun offsetsMap port ->
        Map.add (PortId port.Id) (getDefaultPortPos comp port) offsetsMap)
   
/// Initialise a new symbol with given parameters and initiate ports and their offsets from centre
let initSymbol
    (pos: XYPos)
    (comptype: ComponentType)
    (label: string)
    : Symbol =

    let id = JSHelpers.uuid ()
    let comp = initComponent pos comptype id label identityMatrix
    {
      Pos = pos
      ShowInputPorts = false
      ShowOutputPorts = false
      InWidth0 = None
      InWidth1 = None
      Colour = SymbolColour.Default
      Component = comp
      Transform = identityMatrix
      PortOffsets = getNewPortOffsets comp
      PortLabels = None
      LabelBoundingBox = {X = pos.X - float comp.W/2.0; Y = pos.Y - float comp.H/2.0 - 20.0; H = 20; W = comp.W}
      HighlightLabel = false
      Opacity = 1.0
      Moving = false
    }

/// Initialise symbol from loaded component
let loadSymbol (comp: Component): Symbol =
    let centrePos = posToCentre comp.W comp.H {X=comp.X; Y=comp.Y}
    let transform =
        match comp.SymbolInfo with
        | Some info -> info.Transform
        | None -> identityMatrix
    let portOffsets =
        match comp.SymbolInfo with
        | Some info -> info.PortOffsets
        | None -> getNewPortOffsets comp
    let portLabels =
        match comp.SymbolInfo with
        | Some info -> info.PortLabels
        | None -> None
    let labelBoundingBox =
        match comp.SymbolInfo with
        | Some info ->  info.LabelBoundingBox
        | None -> {
                    X = centrePos.X - float comp.W/2.0
                    Y = centrePos.Y - float comp.H/2.0 - 20.0
                    H = 20
                    W = comp.W
                }
    {
        Pos = centrePos
        ShowInputPorts = false
        ShowOutputPorts = false
        InWidth0 = None
        InWidth1 = None
        Colour = SymbolColour.Default
        Component = comp
        Transform = transform
        PortOffsets = portOffsets
        PortLabels = portLabels
        LabelBoundingBox = labelBoundingBox
        HighlightLabel = false
        Opacity = 1.0
        Moving = false
    }

/// Draw all elements of symbol
let drawSymbol (symDisp: SymbolDisp) (sym: Symbol)  =
    let curvatureLength = 30.0
    let comp = sym.Component
    let h = comp.H
    let w = comp.W

    let coordsToStringSharp (coords: XYPos list) : string =
        ("", coords)
        ||> List.fold (fun points coord -> sprintf "%s %f,%f" points coord.X coord.Y)

    let coordsToStringCurved (coords: (XYPos*XYPos) list) (originalPts: XYPos list) : string =
        let folder (acc: string) ((a,b) : XYPos*XYPos) (pt : XYPos) =
            let curve = sprintf "C %f %f, %f %f, %f %f" pt.X pt.Y pt.X pt.Y a.X a.Y
            let line = sprintf "L %f %f" b.X b.Y
            sprintf "%s %s %s" acc curve line

        let initAcc = 
            coords
            |> List.last
            |> snd
            |> (fun startPt -> 
                  sprintf "M %f %f" startPt.X startPt.Y
               )

        let SVGStr = List.fold2 folder initAcc coords originalPts
        sprintf "%s Z" SVGStr


    let shortenLine ((a,b) : XYPos*XYPos) : XYPos*XYPos =
        let shortenRatio = min (curvatureLength / (euclideanDistance a b)) 0.5
        let newA = {X = (a.X + shortenRatio * (b.X-a.X)) ; Y = (a.Y + shortenRatio * (b.Y-a.Y))}
        let newB = {X = (b.X + shortenRatio * (a.X-b.X)) ; Y = (b.Y + shortenRatio * (a.Y-b.Y))}
        (newA, newB)

    let symCoords : ReactElement list =
        getSymbolCoords comp.Type w h
        |> getPolygonCoords
        |> List.map (transformCoord sym.Transform w h)
        |> (fun points ->
              match symDisp with
              | Sharp ->
                  points
                  |> (fun coords -> 
                        match comp.Type with
                        | MergeWires | SplitWire _ -> ""
                        | _ -> coordsToStringSharp coords)
                  |> (fun coords -> createPolygon coords sym.Colour sym.Opacity false)
              | Curved ->
                  points @ [List.head points]
                  |> List.pairwise
                  |> List.map shortenLine
                  |> (
                        fun coords -> 
                            match comp.Type with
                            | MergeWires | SplitWire _ -> ""
                            | _ -> coordsToStringCurved coords points
                     )
                  |> (fun coords -> createShape coords sym.Colour sym.Opacity false) 
           )
        

    let extraShapeCoords : ReactElement list = 
        let {SPos = shapePos; ColourStroke = cStroke} = getExtraShapes comp w h
        shapePos
        |> List.map (List.map (transformCoord sym.Transform w h))
        |> List.map (fun coords -> coordsToStringSharp coords)
        |> List.collect (fun coords -> createPolygon coords sym.Colour sym.Opacity cStroke)
    
    let extraText : ReactElement list =
        getExtraText sym w h (sym.InWidth0, sym.InWidth1)
        |> List.map (fun ({TPos=txtPos} as textInfo) -> {textInfo with TPos=transformCoord sym.Transform w h txtPos})
        |> List.collect addText

    let labelText = getLabelText sym comp.Label w h |> addText

    [
        symCoords;
        extraShapeCoords;
        extraText;
        labelText;
        drawPortsText (comp.InputPorts @ comp.OutputPorts) (getPortLabels comp.Type) sym;
        drawPorts comp.InputPorts sym.ShowInputPorts sym;
        drawPorts comp.OutputPorts sym.ShowOutputPorts sym;
    ]
    |> List.collect id

/// Initiate the model with no symbols
let init () = 
    {Symbols = Map.empty; CopiedSymbols = Map.empty; Ports = Map.empty; CornerDisp = Sharp}, Cmd.none

//----------------------------View Function for Symbols----------------------------//

/// View for one symbol
/// Using FunctionComponent.Of to improve efficiency (not printing all symbols but only those that are changing)
let private renderSymbol =

    FunctionComponent.Of(
        fun (symbolProps : SymbolDispProps) ->
            symbolProps.Symbols
            |> List.map (fun (symbol:Symbol) ->
                symbol
                |> (drawSymbol symbolProps.SymbolDisp)
                |> g ([ 
                  Style [ 
                      Transform(
                          sprintf "translate(%fpx, %fpx)" (float symbol.Component.X) (float symbol.Component.Y)
                      )
                  ] 
                ]))
            |> g []
        , "Symbol"
        , equalsButFunctions
        )

/// View model by returning SVG for each symbol
let view (model : Model) (dispatch : Msg -> unit) =
    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> Map.toList
    |> List.sortBy (fun (_,sym) -> sym.Moving)
    |> List.map snd
    |> (fun symlist -> {Symbols = symlist; SymbolDisp = model.CornerDisp})
    |> renderSymbol
    |> TimeHelpers.instrumentInterval "SymbolView" start

// ------------------------GET SYMBOLS FUNCTIONS----------------------------------- //

/// Selects the symbols from syms that are associated with the ids in compIds
let filterSymbolsByCompIds
    (compIds: ComponentId list)
    (syms: Map<ComponentId, Symbol>)
    : Map<ComponentId, Symbol> =

    syms |> Map.filter (fun id _ -> List.contains id compIds)

let getSymbolsByCompIds (model: Model) (compIds: ComponentId list) : Symbol list =
    model.Symbols 
    |> filterSymbolsByCompIds compIds
    |> Map.values
    |> Seq.toList

let getCopiedSymbolIds (model: Model) : ComponentId list =
    model.CopiedSymbols
    |> Map.keys
    |> Seq.toList

// ------------------------GET BOUNDING BOXES FUNCTIONS-------------------------------- //

/// Returns true if the component's height/width are flipped based on the symbol transformation
let private areComponentHeightWidthFlipped (sym: Symbol) : bool =
    // Flipped if the transformation changes a line from horizontal to vertical
    {X = 1.0; Y = 0.0}
    |> (*) sym.Transform
    |> (fun p -> p.X = 0)

let computeSymbolBoundingBox (sym: Symbol) : BoundingBox =
    let height, width =
        if areComponentHeightWidthFlipped sym then
            float sym.Component.W, float sym.Component.H
        else
            float sym.Component.H, float sym.Component.W
    {X = sym.Pos.X - width/2.0; Y = sym.Pos.Y - height/2.0; H = height; W = width}

/// Returns the bounding boxes of all symbols in the model
let getSymbolBoundingBoxes (model: Model) : Map<ComponentId, BoundingBox> =
    model.Symbols
    |> Map.map (fun _ sym -> computeSymbolBoundingBox sym)

/// Returns the bounding box of the symbol associated with compId
let getSymbolBoundingBox (model: Model) (compId: ComponentId) : BoundingBox =
    Map.find compId model.Symbols
    |> computeSymbolBoundingBox

/// Returns the bounding boxes of all symbol labels in the model
let getLabelBoundingBoxes (model: Model) : Map<ComponentId, BoundingBox> =
    model.Symbols
    |> Map.map (fun _ sym -> sym.LabelBoundingBox)

/// Returns the bounding box of the symbol associated with compId
let getLabelBoundingBox (model: Model) (compId: ComponentId) : BoundingBox =
    Map.find compId model.Symbols
    |> (fun sym -> sym.LabelBoundingBox)

// --------------------- GETTING PORTS AND THEIR POSITION INTERFACE FUNCTIONS------------------------------- //

/// Returns the port associated with id
let getPort (model: Model) (id: PortId) : Port =
    model.Ports[id]

let getPortsByType (pType: PortType) (sym: Symbol) : Port list =
    match pType with
    | PortType.Input -> sym.Component.InputPorts
    | PortType.Output -> sym.Component.OutputPorts

/// Returns absolute port positions of type pType, that are associated with the symbols in syms
let getPortPositionsByType (pType: PortType) (syms: Symbol list) : Map<PortId, XYPos> =
    let getPortPositions (sym: Symbol) : List<PortId * XYPos> =
        sym
        |> getPortsByType pType
        |> List.map (fun port -> PortId port.Id, sym.Pos + sym.PortOffsets[PortId port.Id])

    syms
    |> List.collect getPortPositions 
    |> Map.ofList

/// Returns both absolute input and output port positions of the ports associated with the ids in compIds
let getInputOutputPortPositions
    (model: Model)
    (compIds: ComponentId list)
    : Map<InputPortId, XYPos> * Map<OutputPortId, XYPos> =

    let syms = getSymbolsByCompIds model compIds

    let inputPortPos =
        syms
        |> getPortPositionsByType PortType.Input
        |> Map.toList
        |> List.map (fun (id, pos) -> id |> portIdToInputPortId, pos)
        |> Map.ofList
    
    let outputPortPos =
        syms
        |> getPortPositionsByType PortType.Output
        |> Map.toList
        |> List.map (fun (id, pos) -> id |> portIdToOutputPortId, pos)
        |> Map.ofList

    inputPortPos, outputPortPos

/// Returns a port attribute, obtained by attFun, of the port specified by portId
let private getPortAtt (model: Model) (attFun: Symbol -> XYPos -> 'a) (portId: PortId) : 'a =
    model.Symbols
    |> Map.pick (fun _ sym ->
        sym.PortOffsets
        |> Map.tryFind portId
        |> Option.map (attFun sym)
    )

/// Returns the absolute position of the port associated with portId
let getPortPosition (model: Model) (portId: PortId) : XYPos =
    getPortAtt model (fun sym offset -> sym.Pos + offset) portId

let getPortDirection (model: Model) (portId: PortId) : PortDirection =
    getPortAtt model getOrientation portId

// --------------------- COPY SYMBOLS FUNCTIONS---------------------------------- //

/// Removes all digits at the end of the string
let removeTerminalDigits (str: string) : string =
    let m = Regex.Match(str, @"(.*?)\d+$")
    if m.Success then m.Groups[1].Value else str

/// Returns the number at the end of the string (consecutive digits).
/// Returns 0 if the string does not end with any digits.
let getTerminalNumber (str: string) : int =
    let m = Regex.Match(str, @"\d+$")
    if m.Success then int m.Value else 0

/// Splits a string into its prefix and terminal digits.
/// Returns 0 for the terminal digits if none exist.
let splitIntoPrefixAndNumber (str: string) : string * int =
    let m = Regex.Match(str, @"(.*?)(\d+)$")
    if m.Success then m.Groups[1].Value, int m.Groups[2].Value else str, 0

/// Returns a mapping from symbol label prefix to a list of existing terminal numbers.
/// For example, labels "a1" and "a3" in syms would appear in the mapping as "a":[1, 3].
let private createPrefixToNumbersMap (syms: Map<ComponentId, Symbol>) : Map<string, int list> =
    syms
    |> Map.values
    |> Seq.toList
    |> List.map (fun sym -> sym.Component.Label |> splitIntoPrefixAndNumber)
    |> List.groupBy fst
    |> List.map (fun (prefix, prefixNumPairs) -> prefix, prefixNumPairs |> List.map snd)
    |> Map.ofList

/// Returns a map from prefix to symbols whose label should start with that prefix.
/// Prefixes are either the current symbol label with the terminal number removed or 
/// the current symbol label with a dot appended when the current label is a custom label
/// that ends with a number. Prefix example: "MUX1" -> "MUX", "CUSTOM8" -> "CUSTOM8.".
let private groupNewSymbolsByPrefix (syms: Symbol list) : List<string * Symbol list> =
    let hasCustomLabelWithNum (sym: Symbol) : bool =
        let prefix = sym.Component.Label |> removeTerminalDigits
        let defaultPrefix = getDefaultPrefix sym.Component.Type
        prefix <> defaultPrefix && prefix <> sym.Component.Label

    syms
    |> List.groupBy (fun sym ->
            if hasCustomLabelWithNum sym then
                sym.Component.Label + "."
            else
                sym.Component.Label |> removeTerminalDigits
        )

/// Generates labels for the new symbols in syms according to the following properties in order of priority:
/// 1. Pasted symbols of the same label prefix are numbered sequentially, where the pasted lowest number
///     symbol is at the same relative position as the copied lowest number symbol and so on
/// 2. Labels get the lowest positive number possible
let generateNewSymbolLabels (model: Model) (syms: Symbol list) : Map<ComponentId, string> =
    let existingLabels = model.Symbols |> createPrefixToNumbersMap

    let generatePrefixGroupLabels ((prefix, syms): string * Symbol list) : List<ComponentId * string> =
        // Prepend 0 so that new labels can start with number 1
        let existingNumsSorted =
            0 :: (Map.tryFind prefix existingLabels |> Option.defaultValue [] |> List.sort)

        let gabIdx =
            existingNumsSorted
            |> List.pairwise
            |> List.map (fun (a, b) -> b - a - 1)
            |> List.tryFindIndex (fun gab -> gab >= syms.Length)
            |> Option.defaultValue (existingNumsSorted.Length - 1)
        let firstNum = existingNumsSorted[gabIdx] + 1

        syms
        |> List.sortBy (fun sym -> sym.Component.Label |> getTerminalNumber)
        |> List.mapi (fun i sym -> ComponentId sym.Component.Id, prefix + string (firstNum + i))

    syms
    |> groupNewSymbolsByPrefix
    |> List.collect generatePrefixGroupLabels
    |> Map.ofList

/// Returns a copy of the symbol with the specified new position and new label
let copySymbol (sym: Symbol) (newPos: XYPos) (newLabel: string) : Symbol =
    let newSym = initSymbol newPos sym.Component.Type newLabel
    let newPortOffsets =
        newSym.PortOffsets
        |> Map.map (fun _ offset -> sym.Transform * offset)
    // Keep the label position changes
    let newLabelBB =
        {sym.LabelBoundingBox with
            X = sym.LabelBoundingBox.X - sym.Pos.X + newSym.Pos.X
            Y = sym.LabelBoundingBox.Y - sym.Pos.Y + newSym.Pos.Y
        }
    {newSym with
        Transform = sym.Transform
        PortOffsets = newPortOffsets
        LabelBoundingBox = newLabelBB
    }
    |> updateSymbolInfo

let addSymbolToModel (model: Model) (sym: Symbol) : Model =
    let newSyms = model.Symbols.Add (ComponentId sym.Component.Id, sym)
    let newPorts = addToPortModel model sym
    {model with Symbols = newSyms; Ports = newPorts}

/// Creates copies of the copied symbols in model.
/// The new symbols are created at the mouse position and have new labels.
/// Returns the updated model and a list of component ids of the pasted symbols.
let pasteSymbols (model: Model) (mousePos: XYPos) : Model * ComponentId list =
    let copiedSyms =
        model.CopiedSymbols
        |> Map.values
        |> Seq.toList
    let baseSym = copiedSyms |> List.minBy (fun sym -> sym.Pos.X)
    let newLabels = generateNewSymbolLabels model copiedSyms

    let pasteSymbol ((model, pastedIds): Model * ComponentId List) (oldSym: Symbol) =
        // Ensure that the pasted symbols maintain the relative position to each other
        // The leftmost symbol (basePos) is moved to the mouse postion
        let posDiff = oldSym.Pos - baseSym.Pos
        let newPos = mousePos + posDiff

        let newSym = copySymbol oldSym newPos newLabels[ComponentId oldSym.Component.Id]
        let newModel = addSymbolToModel model newSym
        newModel, pastedIds @ [ ComponentId newSym.Component.Id ]

    ((model, []), copiedSyms) ||> List.fold pasteSymbol
    
/// Tries to find the pasted ports that correspond to the copied ports specified by copiedInputPortId and copiedOutputPortId.
/// copiedCompIds and pastedCompIds must be of the same length and indices must correspond.
/// copiedCompIds must be in model.CopiedSymbols. pastedCompIds must be in model.Symbols.
let tryFindCorrespondingPastedPorts
    (model: Model)
    (copiedCompIds: ComponentId list)
    (pastedCompIds: ComponentId list)
    (InputPortId copiedInputPortId, OutputPortId copiedOutputPortId)
    : Option<InputPortId * OutputPortId> =

    let tryFindPastedPorts (cCompId, pCompId) =
        let cComp = model.CopiedSymbols[cCompId].Component
        let pComp = model.Symbols[pCompId].Component
        
        let tryFindPastedPort (cPorts: Port list) (pPorts: Port list) (targetId: string) =
            cPorts
            |> List.tryFindIndex (fun port -> port.Id = targetId)
            |> Option.map (fun copiedIdx -> pPorts[copiedIdx].Id)
        
        copiedInputPortId |> tryFindPastedPort cComp.InputPorts pComp.InputPorts,
        copiedOutputPortId |> tryFindPastedPort cComp.OutputPorts pComp.OutputPorts

    let pPortIds =
        (copiedCompIds, pastedCompIds)
        ||> List.zip
        |> List.map tryFindPastedPorts

    let pInputPortId = pPortIds |> List.collect (function | Some i, _ -> [i] | _ -> [])
    let pOutputPortId = pPortIds |> List.collect (function | _, Some o -> [o] | _ -> [])

    match pInputPortId, pOutputPortId with 
    | [pInputPortId], [pOutputPortId] -> Some (InputPortId pInputPortId, OutputPortId pOutputPortId) 
    | _ -> None

// --------------------- UPDATE SYMBOLS FUNCTIONS---------------------------------- //

let private showPorts
    (showInputPorts: bool)
    (showOutputPorts: bool)
    (syms: Map<ComponentId, Symbol>)
    : Map<ComponentId, Symbol> =

    syms
    |> Map.map (fun _ sym -> {sym with ShowInputPorts = showInputPorts; ShowOutputPorts = showOutputPorts})

/// Modifies the symbols in syms that are associated with the ids in compIds using the modifier
let private modifySymbolsByCompIds
    (modifier: Symbol -> Symbol)
    (syms: Map<ComponentId, Symbol>)
    (compIds: ComponentId list)
    : Map<ComponentId, Symbol> =

    (syms, compIds)
    ||> List.fold (fun syms id ->
            let newSym = modifier syms[id]
            Map.add id newSym syms
        )

/// Move a symbol by the amount specified by move
let private moveSymbol (move: XYPos) (sym: Symbol) : Symbol =
    {sym with
        Moving = true
        Pos = {X = sym.Pos.X + move.X; Y = sym.Pos.Y + move.Y}
        Component = {sym.Component with
                        X = sym.Component.X + int move.X
                        Y = sym.Component.Y + int move.Y
                    }
        LabelBoundingBox = {sym.LabelBoundingBox with
                                X = sym.LabelBoundingBox.X + move.X
                                Y = sym.LabelBoundingBox.Y + move.Y
                            }
    }
    |> updateSymbolInfo

/// Move a symbol's label by the amount specified by move
let private moveLabel (move: XYPos) (sym: Symbol) : Symbol =
    {sym with
        LabelBoundingBox = {sym.LabelBoundingBox with
                                X = sym.LabelBoundingBox.X + move.X
                                Y = sym.LabelBoundingBox.Y + move.Y
                            }
    }
    |> updateSymbolInfo

/// Returns a map from a port id to its label, using the default port labels
let private assignLabelsToPorts (sym: Symbol) : Map<PortId, string> =
    let (PortLabels portLabels) = getPortLabels sym.Component.Type
    let transformedLabels =
        (Map.empty, portLabels)
        ||> Map.fold (fun mp portDir labels ->
                Map.add (transformPortDir sym.Transform portDir) labels mp
            )

    let portIdPosPairs =
        sym.Component.InputPorts @ sym.Component.OutputPorts
        |> List.map (fun port ->
                let id = PortId port.Id
                id,
                match Map.tryFind id sym.PortOffsets with
                | Some offset -> offset
                | None -> failwith "Can't find PortId in PortOffsets field of symbol"
            )

    portIdPosPairs
    |> List.groupBy (fun (id, offset) -> getOrientation sym offset)
    |> List.map (fun (direction, idOffsetPairs) ->
        direction,
        idOffsetPairs |> List.map fst
    )
    |> List.collect (fun (portDirection, portIds) ->
            match Map.tryFind portDirection transformedLabels with
            | None -> []
            | Some labels -> List.zip portIds labels // The first port is associated with the first label
        )
    |> Map.ofList

/// Moves the ports belonging to compId along the symbol's edges.
/// If moveRight is true, then the ports are moved to the right along the edges (when looking from the symbol center),
/// and to the left otherwise.
let private movePortsAlongSymbolEdges
    (model: Model)
    (compId: ComponentId)
    (ports: PortId list)
    (moveRight: bool)
    (moveAmount: float)
    : Model =

    let sym = model.Symbols[compId]

    // Port movement requires us to keep track of which port belongs to which label
    let portLabels =
        match sym.PortLabels with
        | Some labels -> labels
        | None -> sym |> assignLabelsToPorts

    let symBB = getSymbolBoundingBox model compId
    let movePort (portOffsets: Map<PortId, XYPos>) (port: PortId) : Map<PortId, XYPos> =
        let portOffset = portOffsets[port]
        let newPortOffset =
            let sign = if moveRight then 1.0 else -1.0
            match getOrientation sym portOffset with
            | PLeft ->
                let newPosY = portOffset.Y - sign * moveAmount
                if abs(newPosY) > symBB.H/2.0 then
                    {X = -symBB.W/2.0 - symBB.H/2.0 - sign * newPosY; Y = -sign * symBB.H/2.0}
                elif abs(newPosY) = symBB.H/2.0 then
                    // We don't want a port on a symbol corner
                    {X = -symBB.W/2.0 + 10.0; Y = -sign * symBB.H/2.0}
                else
                    {X = portOffset.X; Y = newPosY}
            | PRight ->
                let newPosY = portOffset.Y + sign * moveAmount
                if abs(newPosY) > symBB.H/2.0 then
                    {X = symBB.W/2.0 + symBB.H/2.0 - sign * newPosY; Y = sign * symBB.H/2.0}
                elif abs(newPosY) = symBB.H/2.0 then
                    {X = symBB.W/2.0 - 10.0; Y = sign * symBB.H/2.0}
                else
                    {X = portOffset.X; Y = newPosY}
            | PBottom ->
                let newPosX = portOffset.X - sign * moveAmount
                if abs(newPosX) > symBB.W/2.0 then
                    {X = -sign * symBB.W/2.0; Y = symBB.H/2.0 + symBB.W/2.0 + sign * newPosX}
                elif abs(newPosX) = symBB.W/2.0 then
                    {X = -sign * symBB.W/2.0; Y = symBB.H/2.0 - 10.0}
                else
                    {X = newPosX; Y = portOffset.Y}
            | PTop ->
                let newPosX = portOffset.X + sign * moveAmount
                if abs(newPosX) > symBB.W/2.0 then
                    {X = sign * symBB.W/2.0; Y = -symBB.H/2.0 + sign * newPosX - symBB.W/2.0}
                elif abs(newPosX) = symBB.W/2.0 then
                    {X = sign * symBB.W/2.0; Y = -symBB.H/2.0 + 10.0}
                else
                    {X = newPosX; Y = portOffset.Y}
        Map.add port newPortOffset portOffsets

    let newPortOffsets =
        (sym.PortOffsets, ports) ||> List.fold movePort
    let newSym =
        {sym with PortOffsets = newPortOffsets; PortLabels = Some portLabels}
        |> updateSymbolInfo
    {model with Symbols = Map.add compId newSym model.Symbols}

/// Rotate a symbol by 90 degrees
let private rotateSymbol (sym: Symbol) : Symbol =
    let rotationMatrix = getRotationMatrix D90
    let newPortOffsets =
        sym.PortOffsets
        |> Map.map (fun _ offset -> rotationMatrix * offset)

    // Move label vertically so that it stays at the same distance from the symbol
    let sign = if areComponentHeightWidthFlipped sym then -1 else 1
    let yDiff = sign * (sym.Component.H - sym.Component.W) / 2
    let newLabelBB = {sym.LabelBoundingBox with Y = sym.LabelBoundingBox.Y + float yDiff}

    {sym with
        Transform = rotationMatrix * sym.Transform
        PortOffsets = newPortOffsets
        LabelBoundingBox = newLabelBB
    }
    |> updateSymbolInfo

/// Reflect a symbol vertically
let private reflectSymbol (sym: Symbol) : Symbol =
    let newPortOffsets =
        sym.PortOffsets
        |> Map.map (fun _ offset ->  reflectionMatrix * offset)

    {sym with Transform = reflectionMatrix * sym.Transform; PortOffsets = newPortOffsets}
    |> updateSymbolInfo

/// Changes the colour and opacity of the symbols to the default values
let private resetSymbolsColourAndOpacity (syms: Map<ComponentId, Symbol>) : Map<ComponentId, Symbol> =
    syms
    |> Map.map (fun _ sym -> {sym with Colour = SymbolColour.Default; Opacity = SymbolOpacity.Default})

/// Highlights all the labels that contain txt.
/// The case of txt is ignored.
let private highlightLabels (model: Model) (txt: string) : Model =
    let labelContainsTxt (sym: Symbol) : bool =
        sym.Component.Label.ToUpper().Contains (txt.ToUpper())

    let newSymbols =
        model.Symbols
        |> Map.map (fun _ sym ->
            {sym with HighlightLabel = txt <> "" && labelContainsTxt sym}
        )
    {model with Symbols = newSymbols}

/// Modifies the number of bits of the component specified by compId and returns its symbol
let private modifyNumberOfBits (model: Model) (compId: ComponentId) (newBits: int) : Symbol =
    let sym = model.Symbols[compId]
    let newCompType = 
        match sym.Component.Type with
        | Input _ -> Input newBits
        | Output _ -> Output newBits
        | Viewer _ -> Viewer newBits
        | NbitsAdder _ -> NbitsAdder newBits
        | NbitsNot _ -> NbitsNot newBits
        | NbitsAnd _ -> NbitsAnd newBits
        | NbitsOr _ -> NbitsOr newBits
        | NbitsXor _ -> NbitsXor newBits
        | NbitsNand _ -> NbitsNand newBits
        | NbitsNor _ -> NbitsNor newBits
        | NbitsXnor _ -> NbitsXnor newBits
        | Register _ -> Register newBits
        | RegisterE _ -> RegisterE newBits
        | SplitWire _ -> SplitWire newBits
        | BusSelection (_, b) -> BusSelection (newBits, b)
        | BusCompare (_, b) -> BusCompare (newBits, b)
        | Constant1 (_, b, txt) -> Constant1 (newBits, b, txt)
        | _ -> failwithf "modifyNumberOfBits: Called with component of incorrect component type"

    {sym with Component = {sym.Component with Type = newCompType}}

/// Modifies the value associated with the component specified by compId and returns its symbol.
/// For example, this value is the LSB for BusSelection and the compare value for BusCompare.
let private modifyValue (model: Model) (compId: ComponentId) (newLsb: int64) : Symbol =
    let sym = model.Symbols[compId]
    let newCompType = 
        match sym.Component.Type with
        | BusSelection (w, _) -> BusSelection (w, int32 newLsb)
        | BusCompare (w, _) -> BusCompare (w, uint32 newLsb) 
        | Constant1 (w, _, txt) -> Constant1 (w, newLsb, txt)
        | _ -> failwithf "modifyValue: Called with component of incorrect component type"

    {sym with Component = {sym.Component with Type = newCompType}}

/// Modifies the value and text associated with the constant specified by compId and returns its symbol
let private modifyConstant (model: Model) (compId: ComponentId) (newValue: int64) (newText: string) : Symbol=
    let sym = model.Symbols[compId]
    let newCompType = 
        match sym.Component.Type with
        | Constant1 (w, _, _) -> Constant1 (w, newValue, newText)
        | _ -> failwithf "modifyCompConstant: Called with component of incorrect component type"

    {sym with Component = {sym.Component with Type = newCompType}}

/// Modifies the memory associated with the component specified by compId and returns its symbol.
/// The memory is modified by setting the value at the specified address.
let private modifyMemory (model: Model) (compId: ComponentId) (addr: int64) (value: int64) : Symbol =
    let writeMem (mem: Memory1) = {mem with Data = Map.add addr value mem.Data}

    let sym = model.Symbols[compId]
    let newCompType =
        match sym.Component.Type with
        | RAM1 mem -> RAM1 (writeMem mem)
        | AsyncRAM1 mem -> AsyncRAM1 (writeMem mem)
        | ROM1 mem -> ROM1 (writeMem mem)
        | AsyncROM1 mem -> AsyncROM1 (writeMem mem)
        | _ -> failwithf "modifyMemory: Called with component of incorrect component type"
        
    {sym with Component = {sym.Component with Type = newCompType}}

/// Modifies the memory type associated with the memory specified by compId and returns its symbol
let private modifyMemoryType (model: Model) (compId: ComponentId) (newMemoryType: ComponentType) : Symbol =
    let sym = model.Symbols[compId]
    let newCompType =
        match sym.Component.Type with
        | RAM1 _ | AsyncRAM1 _ | ROM1 _ | AsyncROM1 _ -> newMemoryType
        | _ -> failwithf "modifyMemoryType: Called with component of incorrect component type"

    {sym with Component = {sym.Component with Type = newCompType}}

// Check if component has any synchronous components
let private checkSynchronous (compToSetup:LoadedComponent) (ldComps : LoadedComponent list) (comp:Component) : Component=
    match SimulationBuilder.runCanvasStateChecksAndBuildGraph compToSetup.CanvasState ldComps with
    | Error err -> printfn $"Error: {err}" |> (fun _ -> comp)
    | Ok graph ->
        match DependencyMerger.mergeDependencies compToSetup.Name graph compToSetup.CanvasState ldComps with
        | Error err -> printfn $"Error: {err}" |> (fun _ -> comp)
        | Ok graph ->
            graph
            |> SynchronousUtils.hasSynchronousComponents
            |> (fun sync -> 
            { comp with
                IsClocked = sync
            })
    

/// Updates the symbol module based on the message
let update (msg: Msg) (model: Model) : Model*Cmd<'a>  =
    match msg with
    | AddSymbol (pos, compType, label) ->
        let newModel =
            initSymbol pos compType label
            |> addSymbolToModel model
        newModel, Cmd.none

    | DeleteSymbols compIds ->
        let newSyms = (model.Symbols, compIds) ||> List.fold (fun syms id -> Map.remove id syms)
        {model with Symbols = newSyms}, Cmd.none

    | ShowAllInputPorts ->
        let newSyms = model.Symbols |> showPorts true false
        {model with Symbols = newSyms}, Cmd.none

    | ShowAllOutputPorts ->
        let newSyms = model.Symbols |> showPorts false true
        {model with Symbols = newSyms}, Cmd.none

    | ShowPorts compIds ->
        let resetSyms = model.Symbols |> showPorts false false
        let newSyms =
            (resetSyms, compIds)
            ||> modifySymbolsByCompIds (fun sym -> {sym with ShowInputPorts = true; ShowOutputPorts = true})
        {model with Symbols = newSyms}, Cmd.none

    | HideAllPorts ->
        let newSyms = model.Symbols |> showPorts false false
        {model with Symbols = newSyms}, Cmd.none

    | MoveSymbols (compIds, move) ->
        let resetSyms = model.Symbols |> Map.map (fun _ sym -> {sym with Moving = false})
        let newSyms =
            (resetSyms, compIds)
            ||> modifySymbolsByCompIds (moveSymbol move)
        {model with Symbols = newSyms}, Cmd.none

    | MoveLabel (compId, move) ->
        let newSym = model.Symbols[compId] |> moveLabel move
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none

    | MovePorts (compId, ports, moveRight, moveAmount) ->
        let newModel = movePortsAlongSymbolEdges model compId ports moveRight moveAmount
        newModel, Cmd.none

    | RotateSymbol compId ->
        let newSym = model.Symbols[compId] |> rotateSymbol
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none

    | ReflectSymbol compId ->
        let newSym = model.Symbols[compId] |> reflectSymbol
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none

    | CopySymbols compIds ->
        let copiedSyms = model.Symbols |> filterSymbolsByCompIds compIds
        {model with CopiedSymbols = copiedSyms}, Cmd.none

    | PasteSymbols compIds ->
        let resetSyms = model.Symbols |> resetSymbolsColourAndOpacity
        let newSyms =
            (resetSyms, compIds)
            ||> modifySymbolsByCompIds (fun sym -> {sym with Opacity = SymbolOpacity.Paste})
        {model with Symbols = newSyms}, Cmd.none

    | SelectSymbols compIds ->
        let resetSyms = model.Symbols |> resetSymbolsColourAndOpacity
        let newSyms =
            (resetSyms, compIds)
            ||> modifySymbolsByCompIds (fun sym -> {sym with Colour = SymbolColour.Select})
        {model with Symbols = newSyms}, Cmd.none

    | SymbolsHaveError compIds ->
        let resetSyms = model.Symbols |> resetSymbolsColourAndOpacity
        let newSyms =
            (resetSyms, compIds)
            ||> modifySymbolsByCompIds (fun sym -> {sym with Colour = SymbolColour.Error})
        {model with Symbols = newSyms}, Cmd.none

    | ErrorSymbols (errorCompIds, selectCompIds, isDragAndDrop) -> 
        let resetSyms = model.Symbols |> resetSymbolsColourAndOpacity
        let selectSyms =
            (resetSyms, selectCompIds)
            ||> modifySymbolsByCompIds (fun sym ->
                    if isDragAndDrop
                    then {sym with Opacity = SymbolOpacity.DragAndDrop}
                    else {sym with Colour = SymbolColour.Select}
                )
        let newSyms =
            (selectSyms, errorCompIds)
            ||> modifySymbolsByCompIds (fun sym -> {sym with Colour = SymbolColour.Error})
        {model with Symbols = newSyms}, Cmd.none

    | ColorSymbols (compIds, colour) -> 
        let newSyms =
            (model.Symbols, compIds)
            ||> modifySymbolsByCompIds (fun sym -> {sym with Colour = string colour})
        {model with Symbols = newSyms}, Cmd.none

    | ChangeLabel (compId, newLabel) ->
        let oldSym = model.Symbols[compId]
        let newSym = {oldSym with Component = {oldSym.Component with Label = newLabel}}
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none

    | HighlightLabels txt ->
        let newModel = highlightLabels model txt
        newModel, Cmd.none
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newSym = modifyNumberOfBits model compId newBits
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none
    
    | ChangeValue (compId, newValue) ->
        let newSym = modifyValue model compId newValue
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none

    | ChangeConstant (compId, newValue, newText) -> 
        let newSym = modifyConstant model compId newValue newText
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none

    | WriteMemoryLine (compId, addr, newValue) ->
        let newSym = modifyMemory model compId addr newValue
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none

    | WriteMemoryType (compId, newMemType) ->
        let newSym = modifyMemoryType model compId newMemType
        {model with Symbols = Map.add compId newSym model.Symbols}, Cmd.none
    
    | ResetModel -> {model with Symbols = Map.empty; Ports = Map.empty}, Cmd.none
    
    | LoadComponents (comps, ldComp, ldComps) ->
        let idSymPairs = 
            comps 
            |> List.map (checkSynchronous ldComp ldComps)
            |> List.map (fun comp -> ComponentId comp.Id, loadSymbol comp)
        let newModel =
            (model, idSymPairs |> List.map snd)
            ||> List.fold addSymbolToModel
        {newModel with Symbols = idSymPairs |> Map.ofList}, Cmd.none

    | SymbolRenderCurved corners -> 
        {model with CornerDisp = corners}, Cmd.none

    | MouseMsg _ -> model, Cmd.none // Allow unused mouse messages
        
// ----------------------INTERFACE TO ISSIE----------------------------- //

let extractComponent (model: Model) (compId: ComponentId) : Component = 
    model.Symbols[compId].Component

let extractComponents (model: Model) : Component list =
    model.Symbols
    |> Map.values
    |> Seq.toList
    |> List.map (fun sym -> sym.Component)
