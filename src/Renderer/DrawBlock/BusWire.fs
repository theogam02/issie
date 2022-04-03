(*
This module implements wires between symbol ports. Wires can be autorouted, or manually routed by dragging segments.
Moving symbols causes the corresponding wires to move.
Wires are read and written from Issie as lists of wire vertices, whatever teh internal representation is.
*)


module BusWire

open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers

//Static Vars
let minSegLen = 5.

//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

///
type Orientation =  Horizontal | Vertical

///
type SnapPosition = High | Mid | Low

///
type Segment = 
    {
        Id : SegmentId
        Index: int
        Start: XYPos
        End: XYPos
        Autoroute: bool
        Dir: Orientation
        HostId: ConnectionId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateList: list<float * SegmentId>
        Draggable : bool
    }

///
type Wire =
    {
        Id: ConnectionId 
        InputPort: InputPortId
        OutputPort: OutputPortId
        Color: HighLightColor
        Width: int
        Segments: list<Segment>
    }

    with static member stickLength = 16.0


type WireDisp = | Traditional | Radiused | Modern
///
type Model =
    {
        Symbol: Symbol.Model
        WX: Map<ConnectionId, Wire>
        FromVerticalToHorizontalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        FromHorizontalToVerticalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        IntersectionCoordinateList: list<XYPos>
        CopiedWX: Map<ConnectionId, Wire> 
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: list<ConnectionId>
        Notifications: Option<string>
        WireDisplay: WireDisp
        segmentSplitPos: list<XYPos>
    }

//----------------------------Message Type-----------------------------------//

///
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (InputPortId * OutputPortId)
    | BusWidths
    | CopyWires of list<ConnectionId>
    | DeleteWires of list<ConnectionId>
    | SelectWires of list<ConnectionId>
    | UpdateWires of list<ComponentId> * XYPos
    | DragWire of ConnectionId * MouseT
    | ColorWires of list<ConnectionId> * HighLightColor
    | ErrorWires of list<ConnectionId>
    | ResetJumps of list<ConnectionId>
    | MakeJumps of list<ConnectionId>
    | ResetModel // For Issie Integration
    | LoadConnections of list<Connection> // For Issie Integration
    | WireView of WireDisp

//-------------------------Debugging functions---------------------------------//

open System.Diagnostics

let ppSId (sId:SegmentId) =
    sId
    |> (fun (SegmentId x) -> x)
    |> Seq.toList
    |> (fun chars -> chars[0..2])
    |> List.map string
    |> String.concat ""

let ppS (seg:Segment) =
    sprintf $"|{seg.Index}:{ppSId seg.Id}|"

let ppWId (wId:ConnectionId) =
        wId
        |> (fun (ConnectionId x) -> x)
        |> Seq.toList
        |> (fun chars -> chars[0..2])
        |> List.map string
        |> String.concat ""

let ppMaps (model:Model) =
    let mhv = model.FromHorizontalToVerticalSegmentIntersections
    let mvh = model.FromVerticalToHorizontalSegmentIntersections
    let m1 =
        mhv
        |> Map.toList
        |> List.map (fun (sid,lst) ->
            List.map (snd >> ppSId) lst
            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
            |> String.concat ";\n"
    let m2 =
        mvh
        |> Map.toList
        |> List.map (fun (sid,lst) ->
            List.map (snd >> ppSId) lst
            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
            |> String.concat ";\n"
    let jumps =
        model.WX
        |> Map.toList
        |> List.map (fun (wId,w) ->
            sprintf $"Wire: {w.Segments |> List.collect (fun seg -> seg.JumpCoordinateList |> List.map (fun (f, sid) -> ppSId sid))}")
            
    printfn $"\n------------------\nMapHV:\n {m1} \n MapVH\n{m2} \nJumps:\n {jumps}\n------------------\n"



let ppSeg seg (model: Model) = 
        let cid,sid = seg
        let wire = model.WX[cid]
        let sg = List.find (fun (s:Segment) -> s.Id = sid ) wire.Segments
        let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
        sprintf $"""[{ppSId sg.Id}: {pxy sg.Start}->{pxy sg.End}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""

let pp segs (model: Model)= 
    segs
    |> List.map  ( fun seg ->
        let cid,sid = seg
        let wire = model.WX[cid]
        match List.tryFind (fun (s:Segment) -> s.Id = sid ) wire.Segments with
        | Some  sg ->
            let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
            sprintf $"""[{pxy sg.Start}->{pxy sg.End}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""
        | None -> "XX")
    |> String.concat ";"

//-------------------------------Implementation code----------------------------//

let findIntersectionCoordinates (ignoredwires :Wire array) (model: Model) : Model =
    // Puts wires into 1D array
    let wires =
        model.WX
        |> Map.toArray
        |> Array.map (fun (_, w) -> w)

    // Top level wire intersection calculator
    let find_wire_intersections (a: Wire, b: Wire) : XYPos array =

        let find_segment_intersections (sega: Segment, segb: Segment) : XYPos array =

            let is_point_on_seg (pos: XYPos) (seg: Segment) =
                if (pos.X = seg.Start.X && pos.X = seg.End.X) // Segment is Vertical and point is at the correct x coordinate
                   && ((pos.Y >= seg.Start.Y && seg.End.Y >= pos.Y) // and between the ends of the segment in the downward direction
                       || (pos.Y <= seg.Start.Y && seg.End.Y <= pos.Y)) then // or the upward direction
                    true // Then the point is on the segment
                elif (pos.Y = seg.Start.Y && pos.Y = seg.End.Y) // Segment is vertical and point is at the correct y coordinate
                     && ((pos.X >= seg.Start.X && seg.End.X >= pos.X) // and between the ends of the segment in the right direction
                         || (pos.X <= seg.Start.X && seg.End.X <= pos.X)) then // or the left direction
                    true // Then the point is on the segment
                else
                    false // If none of those apply the point is not on the segment

            if sega.Dir = segb.Dir then // Since the convention we use is always putting the marker on the start of a segment if they point the same way they should not have a marker at the beginning
                [||]

            elif is_point_on_seg sega.Start segb then // Cases for combinations of endpoints intersecting a segment
                [| sega.Start |] // If this happens put the marker at the start of the first segment

            elif is_point_on_seg segb.End sega then
                [| segb.End |] // If this happens put the marker at the end of the second segment

            elif is_point_on_seg segb.Start sega then
                [| segb.Start |] // If this happens put the marker at the start of the second segment

            elif is_point_on_seg sega.End segb then
                [| sega.End |] // If this happens put the marker at the start of the first segment

            else
                [||] // If none of those apply do not put a marker anywhere

        List.toArray (List.allPairs a.Segments b.Segments) // Generate all pairs of Segments between two wires
        |> Array.filter (fun (a, b) ->
            if ((a.Start = b.Start) && (a.End = b.End)) // Filter when two segments are in the same place, no markers needed
               || ((a.Start = a.End) || (b.Start = b.End)) then // Filter out zero length segments that pop up sometimes
                false
            else
                true)
        |> Array.collect find_segment_intersections // Find all the intersection points between two wires

    let splitPos =
        wires
        |> Array.filter (fun x -> ignoredwires |> Array.filter(fun y -> (x.Id = y.Id)) |> Array.length = 0) // Filters wires present in the ignore wires array
        |> Array.allPairs wires // Gernerate all combinations of wires
        |> Array.filter (fun (a, b) -> if a = b then false else true) // filter out pairs of the same wire
        |> Array.collect find_wire_intersections // Find all the intersection points between all wires
        |> Array.toList

    { model with segmentSplitPos = splitPos }

/// Wire to Connection
let segmentsToVertices (segList:Segment list) = 
    segList
    |> List.pairwise
    |> List.map (fun (first, second) ->
        {
            X = first.End.X
            Y = first.End.Y
            Autoroute = first.Autoroute && second.Autoroute
        }
    )
    |> (fun middle ->
        {
            X = segList.Head.Start.X
            Y = segList.Head.Start.Y
            Autoroute = true
        }
        :: middle @
        [{
            X = segList[(List.length segList) - 1].End.X
            Y = segList[(List.length segList) - 1].End.Y
            Autoroute = true
        }]
    )

/// Convert a issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments 
        (connId) 
        (verticesList: list<Vertex>) =

    let segmentVertexes = List.pairwise verticesList
    let firstDir = match abs((fst segmentVertexes.Head).X - (snd segmentVertexes.Head).X) < 0.00001 with
                    | true -> Vertical
                    | false -> Horizontal
    
    segmentVertexes |> List.mapi (fun index (startVertex, endVertex) ->
        {
            Id = SegmentId(JSHelpers.uuid())
            Index = index
            Start = {X=startVertex.X; Y=startVertex.Y}
            End = {X=endVertex.X; Y=endVertex.Y}
            Autoroute = startVertex.Autoroute || endVertex.Autoroute
            Dir =
                match index % 2 with
                | 0 -> firstDir
                | _ ->
                    match firstDir with
                    | Horizontal -> Vertical
                    | Vertical -> Horizontal
            HostId  = connId;
            JumpCoordinateList = [];
            Draggable = match index with
                        | 0 -> false
                        | a when a = segmentVertexes.Length - 1 -> false
                        | _ -> true
        }
    )
        

    
//----------------------interface to Issie-----------------------//
/// This function is given a ConnectionId and it
/// converts the corresponding BusWire.Wire type to a
/// Connection type, offering an interface
/// between our implementation and Issie.
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let conn = wModel.WX[cId]
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = conn.Id, conn.InputPort, conn.OutputPort
    {
        Id = strId
        Source = { Symbol.getPort wModel.Symbol (PortId strOutputPort) with PortNumber = None } // None for connections 
        Target = { Symbol.getPort wModel.Symbol (PortId strInputPort) with PortNumber = None } // None for connections 
        Vertices = segmentsToVertices conn.Segments
    } // We don't use vertices

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connectio, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.WX
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)

/// Given three points p, q, r, the function returns true if 
/// point q lies on line segment 'pr'. Otherwise it returns false.
let onSegment (p : XYPos) (q : XYPos) (r : XYPos) : bool = 
    (
        (q.X <= max (p.X) (r.X)) &&
        (q.X >= min (p.X) (r.X)) &&
        (q.Y <= max (p.Y) (r.Y)) &&
        (q.Y >= min (p.Y) (r.Y))
    )
  
///Returns the abs of an XYPos object
let getAbsXY (pos : XYPos) = 
    {X = abs pos.X; Y = abs pos.Y}
  

///Returns a segment with positive Start and End coordinates
let makeSegPos (seg : Segment) =
    {seg with
        Start = getAbsXY seg.Start
        End = getAbsXY seg.End }

/// Given two coordinates, this function returns the euclidean
/// distance between them.
let distanceBetweenTwoPoints (pos1 : XYPos) (pos2 : XYPos) : float =
    euclideanDistance pos1 pos2

/// This function renders the given
/// segment (i.e. creates a ReactElement
/// using the data stored inside it),
/// using the colour and width properties given.
let renderSegments (segments : Segment list) (colour : string) (width : string) (wireDisp : WireDisp) (splitPos : list<XYPos>): ReactElement list = 
    
    let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match wOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let halfWidth = (renderWidth/2.0) - (0.75)
    let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }
    let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

    let segmentJumpHorizontalSize = 9.0
    let segmentJumpVerticalSize = 6.0

    let curvatureRadius = 30.0

    let segmentConnectionRadius = 4.0

    let renderSeglet ((startX, endX) : float * float) (y : float) : list<ReactElement> =
        makeLine startX y endX y lineParameters
        ::
        makeCircle startX y circleParameters
        ::
        [
            makeCircle endX y circleParameters
        ]
    
    let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
        let x, y = intersectionCoordinate.X, intersectionCoordinate.Y

        let startingPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y}
        let startingControlPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
        let endingControlPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
        let endingPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y}

        makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
        ::
        makeCircle startingPoint.X startingPoint.Y circleParameters
        ::
        [
            makeCircle endingPoint.X endingPoint.Y circleParameters
        ]
        
    let renderCurvature startingPoint controlPoint endingPoint =
        makePath startingPoint controlPoint controlPoint endingPoint pathParameters
        ::
        makeCircle startingPoint.X startingPoint.Y circleParameters
        ::
        [
            makeCircle endingPoint.X endingPoint.Y circleParameters
        ]

    let renderSegmentSplits (pos: XYPos) : list<ReactElement> =
        [makeCircle pos.X pos.Y {circleParameters with R = segmentConnectionRadius}]

    /// Calculate points where a segment bends
    /// Return the one closest to point p
    let getNearestFlexPoint (seg : Segment) (p : XYPos) : XYPos =
        let getFlexPoint (a:float) (b:float) (q:float) : float =
            let first, second = (min a b) + curvatureRadius, (max a b) - curvatureRadius
            if first < second then
                if abs (first - q) < abs (second - q) then first else second
            else
                (a+b)/2.0
        match seg.Dir with
        | Horizontal ->
            {X = getFlexPoint seg.Start.X seg.End.X p.X; Y = seg.Start.Y}
        | Vertical ->
            {X = seg.Start.X; Y = getFlexPoint seg.Start.Y seg.End.Y p.Y}

    let getCurvedSegments idx (seg : Segment) : list<ReactElement> = // if showing radiussed notation
                          
        let segCurveElement i = 
            let otherSeg = getNearestFlexPoint segments[i] (midpoint seg.Start seg.End)
            let thisSeg = getNearestFlexPoint seg otherSeg
            let controlPt = closestPoint seg.Start seg.End otherSeg
            thisSeg, renderCurvature otherSeg controlPt thisSeg
        let lastIdx = segments.Length - 1
        let currDir = seg.Dir

        let segletStart, prevCurve =
            match idx with
            | n when n > 0 ->
                match segments[n-1].Dir with
                | d when d = currDir -> seg.Start, []
                | _ -> segCurveElement (n-1)
            | _ -> seg.Start, []

        let segletEnd, nextCurve =
            match idx with
            | n when n = lastIdx -> seg.End, []
            | n ->
                match segments[n+1].Dir with
                | d when d = currDir -> seg.End, []
                | _ -> segCurveElement (n+1)

        match seg.Dir with
        | Horizontal ->
            let y = seg.Start.Y
            prevCurve @ renderSeglet (segletStart.X, segletEnd.X) y @ nextCurve

        | Vertical ->
            let Xa, Ya, Xb, Yb = segletStart.X, segletStart.Y, segletEnd.X, segletEnd.Y
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]


    segments
    |> List.mapi 
        (
          fun idx (seg : Segment) ->
              let y = seg.Start.Y
              match seg.Dir with
              | Horizontal ->  
                  let traditionalElements = // horizontal segment cut into 'seglets'
                        seg.JumpCoordinateList
                        |> List.map fst
                        |> List.append [seg.Start.X; seg.End.X]
                        |> List.sort
                        |> List.pairwise
                        |> List.mapi
                          (
                            fun i (s,e) ->
                              let newS = s + segmentJumpHorizontalSize/2.0
                              let newE = e - segmentJumpHorizontalSize/2.0
                              let lastIdx = seg.JumpCoordinateList.Length - 1 + 2 - 1
                              match i with
                              | 0 -> 
                                  match i with
                                  | x when x = lastIdx -> (s, e)
                                  | _ -> (s, newE)
                              | x when x = lastIdx -> (newS, e)
                              | _ -> (newS, newE)
                          )
                        |> List.collect (fun seglet -> renderSeglet seglet y)
                        |> List.append (
                                        seg.JumpCoordinateList
                                        |> List.map (fun x -> {X = fst x; Y = y})
                                        |> List.collect renderSingleSegmentJump
                                       )

                  let radiusedElements = getCurvedSegments idx seg

                  let modernElements = renderSeglet (seg.Start.X, seg.End.X) y
                      
                  match wireDisp with
                  | Traditional ->
                      g [] traditionalElements
                  | Radiused ->
                      g [] radiusedElements
                  | Modern ->
                      g [] modernElements
        

              | Vertical ->
                  let radiusedElements = getCurvedSegments idx seg
                  let Xa, Ya, Xb, Yb = seg.Start.X, seg.Start.Y, seg.End.X, seg.End.Y
                  let traditionalElements = 
                      makeLine Xa Ya Xb Yb lineParameters
                      ::
                      makeCircle Xa Ya circleParameters
                      ::
                      [
                          makeCircle Xb Yb circleParameters
                      ]
                  
                  match wireDisp with
                  | Traditional
                  | Modern ->
                      g [] traditionalElements
                  | Radiused ->
                      g [] radiusedElements
        )
    |> List.append (
                      match wireDisp with
                      | Modern ->
                          splitPos
                          |> List.collect renderSegmentSplits
                      | _ -> []
                   )
    

///
type WireRenderProps =
    {
        key: string
        Segments: list<Segment>
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortLocation: XYPos
        WireDisplay: WireDisp
        segmentSplitPos: list<XYPos>
    }


// ------------------------------redundant wire memoisation code------------------------------
// this code is not used because React (via Function.Of) does this caching anyway - better tha it can be
// done here
let mutable cache:Map<string,WireRenderProps*ReactElement> = Map.empty

/// not used
let memoOf (f: WireRenderProps -> ReactElement, _, _) =
    (fun props ->
        match Map.tryFind props.key cache with
        | None -> 
            let re = f props
            cache <- Map.add props.key (props,re) cache 
            re
        | Some (props',re) ->  
            if props' = props then re else
                let re = f props
                cache <- Map.add props.key (props,re) cache
                re)
//-------------------------------------------------------------------------------------------

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let renderWireSegmentList : list<ReactElement> =
                renderSegments props.Segments (props.ColorP.Text()) (string props.StrokeWidthP) props.WireDisplay props.segmentSplitPos
            
            let renderWireWidthText : ReactElement =
                let textParameters =
                    {
                        TextAnchor = "left";
                        FontSize = "12px";
                        FontWeight = "Bold";
                        FontFamily = "Verdana, Arial, Helvetica, sans-serif";
                        Fill = props.ColorP.Text();
                        UserSelect = UserSelectOptions.None;
                        DominantBaseline = "middle";
                    }
                let textString = if props.StrokeWidthP = 1 then "" else string props.StrokeWidthP //Only print width > 1
                makeText (props.OutputPortLocation.X+1.0) (props.OutputPortLocation.Y-7.0) (textString) textParameters
            g [] ([ renderWireWidthText ] @ renderWireSegmentList)
        
    , "Wire"
    , equalsButFunctions
    )

///
let MapToSortedList map : Wire list = 

    let filterColor (color: HighLightColor) =
        Map.filter (fun id wire -> wire.Color = color) map
        |> Map.toList
        |> List.map snd

    let listSelected = filterColor HighLightColor.Purple
    let listErrorSelected = filterColor HighLightColor.Brown
    let listErrorUnselected = filterColor HighLightColor.Red
    let listUnSelected = filterColor HighLightColor.DarkSlateGrey
    let listCopied = filterColor HighLightColor.Thistle
    let listWaves = filterColor HighLightColor.Blue

    listUnSelected @ listErrorUnselected @ listErrorSelected @ listSelected @ listWaves @ listCopied
   
let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    let wires1 =
        model.WX
        |> Map.toArray
        |> Array.map snd
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    let wires =
        wires1
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId
                        
                    let outputPortLocation = Symbol.getPortPosition model.Symbol (PortId stringOutId)
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            // TODO I removed the makeSegPos function as negative coordinates are no longer used to
                            // specify that assegment shouldn't be autorouted
                            // Originall this line was here
                            // Segments = List.map makeSegPos wire.Segments
                            // I changed it to:
                            Segments = wire.Segments
                            // if you disagree let me know
                            // Ignacio Bricchi
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                            WireDisplay = model.WireDisplay
                            // TODO: Calculate the segment intersection coords
                            segmentSplitPos = model.segmentSplitPos
                        }
                    singleWireView props)
    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start


///////////////////////
/// Autoroute Start ///
///////////////////////

/// Type to hold more precise information about the direction of a segment than just horizontal vertical
type Direction =
    | Right
    | Down
    | Left
    | Up

let directionOrientation dir =
    match dir with
    | Right | Left -> Horizontal
    | Up | Down -> Vertical

/// Computes the extact direction of a segment based on it's start and end points
/// TODO replace this with making the direction a property of the segment
let getExactDir (seg: Segment) : Direction =
    match seg.Dir with
    | Horizontal -> if seg.Start.X <= seg.End.X then Right else Left
    | Vertical -> if seg.Start.Y <= seg.End.Y then Down else Up

/// This function is given two couples of
/// points that define two line segments and it returns:
/// - Some (x, y) if the two segments intersect;
/// - None if the do not.
/// Asumes that lines have to be perpendicular to intersect
let segmentIntersectsSegmentCoordinates ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : Option<XYPos> =

    let middleX = [p1.X;q1.X;p2.X;q2.X] |> List.sort |> List.item 2
    let middleY = [p1.Y;q1.Y;p2.Y;q2.Y] |> List.sort |> List.item 2
    let possibleIntersection = {X = middleX ; Y = middleY}
    match onSegment p1 possibleIntersection q1 , onSegment p2 possibleIntersection q2 with
    | true, true -> Some possibleIntersection
    | _ -> None
      

/// This funtion is given a bounding box and it returns the coordinates
/// of each of its corners
/// Order: TopLeft, TopRight, BottomLeft, BottomRight
let getBoundingBoxCoordinates (bb : BoundingBox) : XYPos * XYPos * XYPos * XYPos = 
    // Get coordinate information
    let {BoundingBox.X = x; BoundingBox.Y = y} = bb
    let {BoundingBox.H = h; BoundingBox.W = w} = bb
    
    // Compute smallest x and y coordinates and absolute width and height
    // check must be done in case h and w are negative
    let x, y = min x (x+w), min y (y+h)
    let w, h = abs w, abs h

    {X = x; Y = y}, // Top Left
    {X = x+w; Y = y}, // Top Right
    {X = x; Y = y+h}, // Bottom Left
    {X = x+w; Y = y+h} // Bottom Right

/// This function is given a segment and a BoundingBox
/// it returns true if the segment intsersects the box
/// it returns true if the segment is inside the box
/// it returns false if neither of the previous is true
let segmentIntersectsBoundingBox (segIn : Segment) (bb: BoundingBox) : bool =
    // get coordinate information
    let topLeft, topRight, bottomLeft, bottomRight = getBoundingBoxCoordinates bb
    let segStart, segEnd = segIn.Start, segIn.End

    let segPointInBox =
        ((segStart.X >= topLeft.X) && (segStart.X <= bottomRight.X) && (segStart.Y >= topLeft.Y) && (segStart.Y <= bottomRight.Y))
        ||
        ((segEnd.X >= topLeft.X) && (segEnd.X <= bottomRight.X) && (segEnd.Y >= topLeft.Y) && (segEnd.Y <= bottomRight.Y))

    // check if segment intersects each side of box
    let intersectsLeft = segmentIntersectsSegmentCoordinates (segIn.Start, segIn.End) (bottomLeft, topLeft)
    let intersectsRight = segmentIntersectsSegmentCoordinates (segIn.Start, segIn.End) (topRight, bottomRight)
    let intersectsTop = segmentIntersectsSegmentCoordinates (segIn.Start, segIn.End) (topLeft, topRight)
    let intersectsBottom = segmentIntersectsSegmentCoordinates (segIn.Start, segIn.End) (bottomLeft, bottomRight)

    if segPointInBox then
        true
    else
        [intersectsLeft; intersectsRight; intersectsBottom; intersectsTop]
        |> List.tryFind (function | Some _ -> true | None -> false)
        |> (function | Some _ -> true | None -> false)

/// This function is given a point and a segment
/// it returns the distance between them.
let distanceFromPointToSegment (point : XYPos) (segment : Segment) : float =     
    // Get vectors for calculations
    let pointVec = point - segment.Start
    let segVec = segment.End - segment.Start

    // special check if segment has length 0
    let segLengthSquared = magnitudeSquared segVec
    if segLengthSquared = 0
    then euclideanDistance point segment.Start
    else 
        // project point onto segment line and clamp to fit within segment
        let pointProjectionFactor = clamp 0 1 ((pointVec * segVec) / segLengthSquared)
        let pointProjection = segment.Start + segVec * pointProjectionFactor
        // compute distance from projected point to original point
        euclideanDistance point pointProjection

///Finds the closest segment in a wire to a point using euclidean distance
let getClosestSegment (model : Model) (wireId : ConnectionId) (pos : XYPos) : Segment =
    model.WX[wireId].Segments
    |> List.minBy (
        fun seg -> 
            distanceFromPointToSegment pos seg)

/// This function takes a segment, the first and last segments in its wire, and a distance to move the segment
/// it returns a new distance to move the segment to prevent the wires moving into components
let getSafeDistanceForMove (seg: Segment) (firstSeg:Segment) (lastSeg:Segment) (distance:float) =
    // extreme float values
    let minFloat = float System.Single.MinValue
    let maxFloat = float System.Single.MaxValue
    
    // clamp information based only on first segment
    let firstSegClamp =
        match getExactDir firstSeg with
        | Right -> firstSeg.Start.X + Wire.stickLength, maxFloat
        | Left -> minFloat, firstSeg.Start.X - Wire.stickLength
        | Up -> minFloat, firstSeg.Start.Y - Wire.stickLength
        | Down -> firstSeg.Start.Y + Wire.stickLength, maxFloat

    // clamp information based only on last segment
    let lastSegClamp =
        match getExactDir lastSeg with
        | Right -> minFloat, lastSeg.End.X - Wire.stickLength
        | Left -> lastSeg.End.X + Wire.stickLength, maxFloat
        | Up -> lastSeg.End.Y + Wire.stickLength, maxFloat
        | Down -> minFloat, lastSeg.End.Y - Wire.stickLength

    // generate clamp values for the final coordinate 
    let coordinateClamps =
        // check if the segment connects to both first and last segment
        if seg.Start = firstSeg.End && seg.End = lastSeg.Start
        then
            max (fst firstSegClamp) (fst lastSegClamp),
            min (snd firstSegClamp) (snd lastSegClamp)
        // check if the segment connects to the first segment
        else if seg.Start = firstSeg.End
        then
            firstSegClamp
        // check if the segment connects to the last segment
        else if seg.End = lastSeg.Start
        then
            lastSegClamp
        // otherwise no clamp
        else
            (minFloat, maxFloat)

    // check the value of the coordinate if we were to use the given distance
    // clamp it given our computed range
    // and return distance to clamped coordinate
    match seg.Dir with
    | Horizontal ->
        let newY = seg.Start.Y + distance
        let clampedY = clamp (fst coordinateClamps) (snd coordinateClamps) newY
        clampedY - seg.Start.Y
    | Vertical ->
        let newX = seg.Start.X + distance
        let clampedX = clamp (fst coordinateClamps) (snd coordinateClamps) newX
        clampedX - seg.Start.X

///Returns the wires connected to a list of components given by componentIds
let getConnectedWires (wModel : Model) (compIds : list<ComponentId>) =
    let inputPorts, outputPorts = Symbol.getInputOutputPortPositions wModel.Symbol compIds

    wModel.WX
    |> Map.toList
    |> List.map snd
    |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts || Map.containsKey wire.OutputPort outputPorts)
    |> List.map (fun wire -> wire.Id)
    |> List.distinct

/// this function returns a new distance if it detects that there is a nearbye wire to clip to
let getDistanceToClip (model: Model) (seg: Segment) (distance: float)=
    let clipDistance = Wire.stickLength
    
    let allWiresFromPort =
        model.Symbol.Symbols
        |> Map.filter (
            fun _  s ->
                s.PortOffsets.TryFind (portIdFromInputOutputPortId model.WX[seg.HostId].OutputPort)
                |> Option.isSome
            )
        |> Map.keys
        |> Seq.toList
        |> (fun l -> [l.Head])
        |> getConnectedWires model
        |> List.filter (fun id -> id <> seg.HostId)
        |> List.map (fun id -> model.WX[id])
    
    let segmentsToClipWith = 
        allWiresFromPort
        |> List.map (
            fun wire ->
                let clipableSegments =
                    wire.Segments
                    |> List.filter(
                        fun s ->
                            match seg.Dir with
                            | Horizontal ->
                                let newY = seg.Start.Y + distance
                                abs(s.Start.X - seg.Start.X) < 0.0001
                                &&
                                abs(s.Start.Y - newY) < clipDistance
                                &&
                                abs(s.End.Y - newY) < clipDistance
                            | Vertical ->
                                let newX = seg.Start.X + distance
                                abs(s.Start.Y - seg.Start.Y) < 0.0001
                                &&
                                abs(s.Start.X - newX) < clipDistance
                                &&
                                abs(s.End.X - newX) < clipDistance
                    )
                match clipableSegments with
                | [] -> None
                | h::_ -> Some h
                
        )
        |> List.filter Option.isSome
        |> List.map Option.get

    match segmentsToClipWith with
    | [] -> distance
    | s::_ ->
        match seg.Dir with
        | Horizontal ->
            s.Start.Y - seg.Start.Y            
        | Vertical ->
            s.Start.X - seg.Start.X

/// This function allows a wire segment to be moved a given amount in a direction perpedicular to
/// its orientation (Horizontal or Vertical). Used to manually adjust routing by mouse drag.
/// The moved segment is tagged by seting autoroute to false so that it cannot be auto-routed
/// after the move, thus keeping the moved position.
let moveSegment (seg:Segment) (distance:float) (model:Model) = 
    let wire = model.WX[seg.HostId]
    let index = seg.Index
    if index <= 0 || index >= wire.Segments.Length - 1 then
        failwithf $"Buswire segment index {index} out of range in moveSegment in wire length {wire.Segments.Length}"
    let prevSeg = wire.Segments[index-1]
    let nextSeg = wire.Segments[index+1]
    if seg.Dir = prevSeg.Dir || seg.Dir = nextSeg.Dir then
        wire
    else
        //runTestFable()
        distance
        |> getSafeDistanceForMove seg wire.Segments.Head wire.Segments[wire.Segments.Length-1]
        |> getDistanceToClip model seg
        |> (fun distance' ->
            let newPrevEnd, newSegStart, newSegEnd, newNextStart = 
                match seg.Dir with

                | Vertical -> 
                    {prevSeg.End with X = seg.Start.X + distance'}, 
                    {seg.Start with X = seg.Start.X + distance'}, 
                    {seg.End with X = seg.End.X + distance'}, 
                    {nextSeg.Start with X = seg.End.X + distance'}

                | Horizontal -> 
                    {prevSeg.End with Y = seg.Start.Y + distance'}, 
                    {seg.Start with Y = seg.Start.Y + distance'}, 
                    {seg.End with Y = seg.End.Y + distance'}, 
                    {nextSeg.Start with Y = seg.End.Y + distance'}

            let newPrevSeg = {prevSeg with End = newPrevEnd}
            let newSeg = {seg with Start = newSegStart;End = newSegEnd;Autoroute=false}
            let newNextSeg = {nextSeg with Start = newNextStart}
        
            let newSegments =
                wire.Segments[.. index-2] @ [newPrevSeg; newSeg; newNextSeg] @ wire.Segments[index+2 ..]

            {wire with Segments = newSegments})

/// Initialisatiton with no wires
let init () =
    let symbols,_ = Symbol.init()
    {   
        WX = Map.empty;
        FromVerticalToHorizontalSegmentIntersections = Map.empty;
        FromHorizontalToVerticalSegmentIntersections = Map.empty;
        IntersectionCoordinateList = []
        Symbol = symbols; 
        CopiedWX = Map.empty; 
        SelectedSegment = SegmentId(""); 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
        WireDisplay = Traditional
        segmentSplitPos = []
    } , Cmd.none

///Returns a tuple of: wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
let filterWiresByCompMoved (wModel : Model) (compIds : list<ComponentId>) =
        let inputPorts, outputPorts = Symbol.getInputOutputPortPositions wModel.Symbol compIds
        let lst = 
            wModel.WX
            |> Map.toList
            |> List.map snd

        let inputWires =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        let outputWires =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.OutputPort outputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        let fullyConnected =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts && Map.containsKey wire.OutputPort outputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        (inputWires, outputWires, fullyConnected)

/// reverse segment order, and Start, End coordinates, so list can be processed from input to output
/// this function is self-inverse
let revSegments (segs:Segment list) =
    List.rev segs
    |> List.map (fun seg -> {seg with Start = seg.End; End = seg.Start})

/// This function takes in:
///     - a starting posistion
///     - a starting direction
///     - an ending posistion
///     - an ending direction
/// 
/// The function transforms the posistioins and directions relative to the starting position and direction
/// so that the relative starting posistion and direction are (0,0) and Right
/// 
/// The function returns the relative ending posistion and direction after the transformation
/// Aswell as the rotation matrix and translation vector required to undo the original transformation
/// 
/// This is useful as we can compute all routing scenarios given a starting position and direction of (0,0) and Right
/// And use the symetry of rotation and translation to automatically generate the rest
let normalizeCoordinates (startPos: XYPos) (startDir: Direction) (endPos: XYPos) (endDir: Direction): Rotation: Mat2x2 * Translation: XYPos * RelativeEndPos: XYPos * RelativeDirection: Direction =
    // center coordinates so start is at 0
    let dx, dy = startPos.X, startPos.Y
    let endPos' = {X = endPos.X - dx; Y = endPos.Y - dy}

    let inverseTranslationVector = {X = dx; Y = dy}

    // rotate coordinates and endDir so start direction is Right
    let directionToNum dir =
        match dir with 
        | Right -> 0
        | Down -> 1
        | Left -> 2
        | Up -> 3
    let numToDir num =
        match num with
        | 0 -> Right
        | 1 -> Down
        | 2 -> Left
        | 3 -> Up
        | _ -> failwithf "Invalid direction %d" num
    let endDir' = 
        (directionToNum endDir - directionToNum startDir + 4) % 4
        |> numToDir
    
    // Compute cos and sin of rotation angle
    let (cost, sint) = 
        match startDir with
        | Right -> (1,0)
        | Down -> (0,-1)
        | Left -> (-1,0)
        | Up -> (0,1)
    
    // generate roation matrixes
    let roationMatrix: Mat2x2 = {X = {X = cost; Y = sint}; Y = {X = -sint; Y = cost}}
    let inverseRotationMatrix: Mat2x2 = {X = {X = cost; Y = -sint}; Y = {X = sint; Y = cost}}

    // rotate endPos
    let endPos'' = roationMatrix * endPos'
    
    (inverseRotationMatrix, inverseTranslationVector, endPos'', endDir')

/// Extend the begining of a wire to include the minimum fixed segment
let prependFixedSegment (dir: Direction) (segs: Segment List) : Segment List =
    let head = segs.Head

    // compute new head segment based on direction of the head wire
    let head' =
        match dir with
        | Right -> {head with Start = {X = head.Start.X - Wire.stickLength; Y = head.Start.Y}}
        | Down -> {head with Start = {X = head.Start.X; Y = head.Start.Y - Wire.stickLength}}
        | Left -> {head with Start = {X = head.Start.X + Wire.stickLength; Y = head.Start.Y}}
        | Up -> {head with Start = {X = head.Start.X; Y = head.Start.Y + Wire.stickLength}}
    head'::segs.Tail

/// Extend the end of a wire to include the minimum fixed segment
let appendFixedSegment (dir: Direction) (segs: Segment List) : Segment List =
    let (front, last) = List.splitAt (List.length segs - 1) segs
    let last = last.Head

    // compute the new last segment based on direction of the last wire
    let last' =
        match dir with
        | Right -> {last with End = {X = last.End.X + Wire.stickLength; Y = last.End.Y}}
        | Down -> {last with End = {X = last.End.X; Y = last.End.Y + Wire.stickLength}}
        | Left -> {last with End = {X = last.End.X - Wire.stickLength; Y = last.End.Y}}
        | Up -> {last with End = {X = last.End.X; Y = last.End.Y - Wire.stickLength}}
    front @ [last']
         
/// This function takes in
///   - a connection ID
///   - A starting Position and Direction
///   - An ending position and direction
///
/// From these it generates and returns a list of segments that connects the two points together in
/// the shortest possible way
let generateSegmentsFromCoordinates (connId: ConnectionId) (startPos: XYPos) (startDir: Direction) (endPos: XYPos) (endDir: Direction): Segment List =
    // normalize coordinates to minimize possible cases
    let (rotationMatrix, translationVector, endPos, endDir) = normalizeCoordinates startPos startDir endPos endDir
    let dx = endPos.X
    let dy = endPos.Y

    // There are 7 cases, depending on the adjusted end direction and end coordinates
    // compute the relative distance for each section of the wire
    let relSegs: float list =
        match endDir with
        | Right ->
            // check if short or long wire is needed
            if dx >= 0
            // handle generating short wire
            then [0;0; dx/2.0; dy; dx/2.0; 0;0]
            // handle generating long wire
            else [0; dy/2.0; dx; dy/2.0; 0]
        | Up | Down ->
            // check which wire configuration to use
            let position_check = 
                match endDir with
                | Up -> dx < 0, dy > 0
                | Down -> dx < 0, dy < 0
                | _ -> failwithf "Unreachable"
            match position_check with
            | (true, true) -> [0; dy; dx; 0;]
            | (true, false) -> [0; dy/2.0; dx; dy/2.0; 0;0]
            | (false, true) -> [0;0; dx/2.0; dy; dx/2.0; 0]
            | (false, false) -> [0;0; dx; dy; 0;0]
        | Left -> 
            // check what side needs the use of min length
            if dx < 0
            then [0; dy; dx; 0;0;]
            else [0;0; dx; dy; 0]

    // function takes in a previous wire and a relative distance
    // it returns a new segment with a perpendicular direction to the previous wire
    // and a new coordinate based on the relative distance
    let nextSeg (seg: Segment) (distance: float) : Segment =
        // get new direction, and coordinate for the new segment
        let dir, x, y =
            match seg.Dir with
            | Horizontal -> Vertical, seg.End.X, seg.End.Y + distance
            | Vertical -> Horizontal, seg.End.X + distance, seg.End.Y
        let index = seg.Index + 1

        // create new segment
        {
            Id = SegmentId(JSHelpers.uuid())
            Index = seg.Index + 1
            Start = {X=seg.End.X;Y=seg.End.Y};
            End = {X=x;Y=y};
            Autoroute = true
            Dir = dir
            HostId = connId;
            JumpCoordinateList = [];
            Draggable =
                match index with
                | 0 -> false
                | a when a = relSegs.Length - 1 -> false
                | _ -> true
        }
    
    // apply rotation and translation to the segments
    let transform (seg: Segment) : Segment =
        {seg with
            Dir =
                match startDir with
                | Left | Right -> seg.Dir
                | Up | Down ->
                    match seg.Dir with
                    | Horizontal -> Vertical
                    | Vertical -> Horizontal
            Start = rotationMatrix * seg.Start + translationVector;
            End = rotationMatrix * seg.End + translationVector
        }

    // create an dummy segment to start the scan
    let dummySeg =
        {
            Id = SegmentId(JSHelpers.uuid())
            Index = -1
            Start = {X=0;Y=0};
            End = {X=0;Y=0};
            Autoroute = true
            Dir = Vertical //match startDir with |Left|Right->Vertical |Up|Down->Horizontal
            HostId = connId;
            JumpCoordinateList = [];
            Draggable = false
        }

    // scan through the relative segments to create absolute segments
    (dummySeg, relSegs)
    ||> List.scan nextSeg
    |> List.tail
    |> List.map transform

/// Get distance required to move segment to avoid any symbols
/// This function takes in a list of symbol bounding boxes
/// and returns an option containing the distance required to move the segment
/// if no movement is needed none is returned
let moveSegmentToAvoidSymbolsDistance (avoidanceDir : Direction * Direction) (symbolBBs : BoundingBox List) (seg: Segment) : float Option =
    let safeSimbolDistance = Wire.stickLength
    
    if abs(seg.Start.X - seg.End.X) < 0.0001 && abs(seg.Start.Y - seg.End.Y) < 0.0001
    then None
    else
        let intersectingBoundingBoxes =
            symbolBBs
            |> List.filter (segmentIntersectsBoundingBox seg)


        // calculate the delta to move for the first intersecting bounding box
        match intersectingBoundingBoxes with
        | [] -> None
        | bb::_ ->
            //printfn "BB: %A" bb
            //printfn "Segment: %A" seg
            match seg.Dir with
            | Horizontal ->
                // get the delta vertical of the boudning box
                if fst avoidanceDir = Up
                then
                    bb.Y - seg.End.Y - safeSimbolDistance
                else
                    bb.Y + bb.H - seg.End.Y + safeSimbolDistance
            | Vertical ->
                // get the delta horizontal of the boudning box
                if snd avoidanceDir = Right
                then
                    bb.X + bb.W - seg.End.X + safeSimbolDistance
                else
                    bb.X - seg.End.X - safeSimbolDistance
            |> Some

/// Update segment list to avoid symbols
/// Recursive function that takes in a list of symbol bounding boxes
/// and a segment list and returns a new segment list with segments moved to avoid symbols
let rec moveSegmentsToAvoidSymbols (avoidanceDir : Direction * Direction) (symbolBBs : BoundingBox list) (segs: Segment list) : Segment list =    
    match segs with
    | prev::seg::next::tail ->
        let distance = moveSegmentToAvoidSymbolsDistance avoidanceDir symbolBBs seg
        match distance with
        | None ->
            let out = prev::moveSegmentsToAvoidSymbols avoidanceDir symbolBBs (seg::next::tail)
            out
        | Some delta ->
            let XYDelta =
                match seg.Dir with
                | Horizontal -> {X=0;Y=delta}
                | Vertical -> {X=delta;Y=0}
            let prev' = {prev with End = prev.End + XYDelta}
            let seg' = {seg with Start = seg.Start + XYDelta; End = seg.End + XYDelta}
            let next' = {next with Start = next.Start + XYDelta}
            let out = moveSegmentsToAvoidSymbols avoidanceDir symbolBBs (prev'::seg'::next'::tail)
            out
    | _ ->
        let out = segs
        out

/// This function checks possible symbol avoidance for segment list
/// And selects if it is possible to avoid symbols
/// Otherwise it returns the original segment list
let selectivelyAvoidSymbols (symbolBBs : BoundingBox list) (segs: Segment List) : Segment list =
    let safeSimbolDistance = Wire.stickLength * Wire.stickLength
    
    // check direction of head and tail
    let startSeg = segs.Head
    let endSeg = segs.[(List.length segs) - 1]
    
    // check each possible avoidance routing
    let movedSegsUpRight = moveSegmentsToAvoidSymbols (Up, Right) symbolBBs segs
    let movedSegsDownRight = moveSegmentsToAvoidSymbols (Down, Right) symbolBBs segs
    let movedSegsDownLeft = moveSegmentsToAvoidSymbols (Down, Left) symbolBBs segs
    let movedSegsUpLeft = moveSegmentsToAvoidSymbols (Up, Left) symbolBBs segs
    
    // function checks if segmets generated are in the same direction as original list
    let checkMovedAndSegAreInSameDir movedSegs =
        List.zip segs movedSegs
        |> List.filter(
            fun (s1, s2) ->
                (getExactDir s1) <> (getExactDir s2)
                &&
                magnitude(s1.Start - s1.End) > 10
        )
        |> List.length = 0
    
    // check if the distance between ports is abve a threshold
    if magnitude (startSeg.Start - endSeg.End) > safeSimbolDistance
    then 
        // check each reroute
        [movedSegsUpRight; movedSegsDownRight; movedSegsDownLeft; movedSegsUpLeft]
        |> List.filter checkMovedAndSegAreInSameDir
        |> (fun l ->
                match l with
                | [] -> segs
                | movedSegs::_ -> movedSegs
        )
    else
        segs


/// Translates port direction to orientation
let flipPortDirection (pd: PortDirection) : PortDirection =
    match pd with
    | PRight -> PLeft
    | PLeft -> PRight
    | PTop -> PBottom
    | PBottom -> PTop

/// Translates port direction to orientation
let portDirectionToDirection (pd: PortDirection) : Direction =
    match pd with
    | PRight -> Right
    | PLeft -> Left
    | PTop -> Up
    | PBottom -> Down

/// Returns a newly autorouted wire given a model and wire
let autorouteWire (model : Model) (wire : Wire) : Wire =
    // get input and output posistion from symbol
    let inputPos =
        wire.InputPort
        |> portIdFromInputOutputPortId
        |> Symbol.getPortPosition model.Symbol
    let outputPos =
        wire.OutputPort
        |> portIdFromInputOutputPortId
        |> Symbol.getPortPosition model.Symbol

    // get input and output direction from symbol
    let inputDir =
        wire.InputPort
        |> portIdFromInputOutputPortId
        |> Symbol.getPortDirection model.Symbol
        |> flipPortDirection
        |> portDirectionToDirection
    let outputDir =
        wire.OutputPort
        |> portIdFromInputOutputPortId
        |> Symbol.getPortDirection model.Symbol
        |> portDirectionToDirection

    // add padding before passing to wire generation so that the wire is not too close to the edge of the symbol
    let inputPadding =
        match inputDir with
        | Right -> {X = -Wire.stickLength; Y = 0}
        | Down -> {X = 0; Y = -Wire.stickLength}
        | Left -> {X = Wire.stickLength; Y = 0}
        | Up -> {X = 0; Y = Wire.stickLength}
    
    let outputPadding =
        match outputDir with
        | Right -> {X = Wire.stickLength; Y = 0}
        | Down -> {X = 0; Y = Wire.stickLength}
        | Left -> {X = -Wire.stickLength; Y = 0}
        | Up -> {X = 0; Y = -Wire.stickLength}

    let middleInputPos = inputPos + inputPadding
    let middleOutputPos = outputPos + outputPadding

    // generate segments from the padded coordinates
    let middleSegments = generateSegmentsFromCoordinates wire.Id middleOutputPos outputDir middleInputPos inputDir

    // get bounding boxes of all symbols
    let symbolBBs =
        model.Symbol
        |> Symbol.getSymbolBoundingBoxes
        |> Map.values
        |> Seq.toList

    // extend coordinates to include the minimum fixed segment
    let segments =
        middleSegments
        |> prependFixedSegment outputDir
        |> appendFixedSegment inputDir
        |> selectivelyAvoidSymbols symbolBBs
    
    {wire with Segments = segments}

//
//  ====================================================================================================================
//
//                                        WIRE SEGMENTS FOR ROUTING
//
//
// Segments, going from Start (output port) to End (input port) coords, are summarised as:
// H => Horizontal (incr X)
// V => Vertical (incr Y)
//
// segment qualifiers:
// F => min length (next to output or input, cannot be shortened)
//
// To determine adjustment on End change we just reverse the segment and apply the Start change algorithm
// Adjustment => reverse list of segments, swap Start and End
//
// ======================================================================================================================

/// function takes in a list of segments and and a new port posistion
/// it returns None if full autoroute is required
/// it returns Some (index) if partial auto route is required where index is the index up to which rerouting is required
let requiresPartialAutoRoute (segs: Segment List) (newPortPos: XYPos) (newPortDir: Direction) : int Option =
    // get the index of the last non dragged segment
    let lastAutoIndex =
        segs
        |> List.takeWhile (fun seg -> seg.Autoroute)
        |> List.length
        |> (fun n -> if n = segs.Length then None else Some n)

    // ensures that the ports are the same direction
    let checkPort index =
        if segs.Head.Dir = directionOrientation newPortDir
        then Some index
        else None

    // enusres that the rerouted part of the segments maintain same general shape
    // if not returns None to indicate a full rerouting is required
    let checkTopology index =
        let topology (pos1: XYPos) (pos2:XYPos) =
            sign (pos1.X - pos2.X), sign (pos1.Y - pos2.Y)
        
        let fixedSeg = segs[index]
        
        let fixedStickDelta =
            match getExactDir segs.Head with
            | Right -> {X = Wire.stickLength; Y = 0}
            | Down -> {X = 0; Y = -Wire.stickLength}
            | Left -> {X = -Wire.stickLength; Y = 0}
            | Up -> {X = 0; Y = Wire.stickLength}

        let originalStart = segs.Head.Start + fixedStickDelta
        let newStart = newPortPos + fixedStickDelta
        let endPos = fixedSeg.End

        if topology originalStart endPos = topology newStart endPos
        then Some index
        else None
    
    lastAutoIndex
    |> Option.bind checkPort
    |> Option.bind checkTopology

/// this function take in the index up to which autrouting is done
/// it returns the updated segments after adjustment
let reRouteSegmentsUpto (segs: Segment list) (newPortPos: XYPos) (index: int) : Segment List =
    // get sections of wire for regeneration
    // - shiftSegs will just be shifted relative to new PortPosition
    // - preFixedSeg will be shifted and extended to ensure that fixed
    //   segments are not shifted
    // - postFixedSeg will be extended only to enusre that fixed segments are not shifted
    // - postFixed segments will not be changed at all
    let shiftSegs, preFixedSeg, fixedSeg, constSegs =
        match List.splitAt (index - 1) segs with
        | shiftSegs, preFixedSeg::fixedSeg::constSegs ->
            shiftSegs, preFixedSeg, fixedSeg, constSegs
        | _ -> failwithf "partialAutoRoute: ends of wire segments cannot be fixed."

    // get the new position delta by checking the original position of the port position
    // check if shiftSegs is empty and use preFixedSeg if so
    let delta = 
        match shiftSegs with
        | head::_ -> newPortPos - head.Start
        | _ -> newPortPos - preFixedSeg.Start

    // get the shift that will be appleid to the segments being extneded
    let fixedShift =
        match fixedSeg.Dir with
        | Horizontal -> {X=delta.X; Y=0}
        | Vertical -> {X=0; Y=delta.Y}
    
    // apply appropriate shift and extensions to each section
    let shiftSegs' = shiftSegs |> List.map (fun seg -> {seg with Start = seg.Start + delta; End = seg.End + delta})
    let preFixedSeg' = {preFixedSeg with Start = preFixedSeg.Start + delta; End = preFixedSeg.End + fixedShift}
    let fixedSegs' = {fixedSeg with Start = fixedSeg.Start + fixedShift}

    // recombine results
    shiftSegs' @ preFixedSeg'::fixedSegs'::constSegs


///Moves a wire by a specified amount by adding a XYPos to each start and end point of each segment
let moveWire (wire : Wire) (diff : XYPos) =    
    {wire with 
        Segments = 
            wire.Segments
            |> List.map (fun seg -> 
                {seg with
                    Start = seg.Start + diff
                    End = seg.End + diff
                })
    }

/// Re-routes a single wire in the model when its ports move.
/// Tries to preserve manual routing when this makes sense, otherwise re-routes with autoroute.
/// Partial routing from input end is done by reversing segments and and swapping Start/End
/// inout = true => reroute input (target) side of wire.
let updateWire (model : Model) (wire : Wire) (inOut : bool) =
    let newPortPos, newPortDir = 
        match inOut with
        | true -> 
            let portID = portIdFromInputOutputPortId wire.InputPort
            Symbol.getPortPosition model.Symbol portID,
            Symbol.getPortDirection model.Symbol portID |> flipPortDirection |> portDirectionToDirection
        | false -> 
            let portID = portIdFromInputOutputPortId wire.OutputPort
            Symbol.getPortPosition model.Symbol portID,
            Symbol.getPortDirection model.Symbol portID |> portDirectionToDirection

    // check what port is being moved
    if inOut then
        // invert the segments before rerouting to treat the new port as the begining of the wire
        // this is how reRouteSegmentsUpto works
        let reversedSegments = revSegments wire.Segments

        // check if partial autoroute is required
        requiresPartialAutoRoute reversedSegments newPortPos newPortDir
        // compute autorute if necessary
        |> Option.map (reRouteSegmentsUpto reversedSegments newPortPos)
        |> Option.map revSegments
    else 
        // check if partial autoroute is required
        requiresPartialAutoRoute wire.Segments newPortPos newPortDir
        // compute autoroute if necessary
        |> Option.map (reRouteSegmentsUpto wire.Segments newPortPos)
    // create wire from segments
    |> Option.map (fun segs -> {wire with Segments = segs})
    // if partial autoroute was not done, do full autoroute
    |> Option.defaultValue (autorouteWire model wire)

/////////////////////
 // Autoroute End //
/////////////////////


/// Calculates and adds wire jumps to model.
let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =

    // Create a new WX to hold the updated version
    let mutable newWX = model.WX

    // Converts the ConnectionID list to an array because arrays are faster to iterate over
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps

    //  Function to Add the jumps to the wire and segment specified by wireID and segmentID
    let setJumpsForSegmentInWire (wireID : ConnectionId) (segmentID : int) (jumps: (float * SegmentId) list) =

        //  Sort Jumps
        let jumps = List.sortDescending jumps

        // Changes the segment with index segmentID
        let segmentsWithJumpSet =
            List.mapi (fun i x -> if i <> segmentID then x else { x with JumpCoordinateList = jumps }) newWX[wireID].Segments

        // Adds the changes to the new iteration of WX
        newWX <- Map.add wireID { newWX[wireID] with Segments = segmentsWithJumpSet } newWX
    
    //  Extracts the segments from the model into a 2D array
    let segs =
        model.WX
        |> Map.toArray
        // _ to indicate dummy variable and change it to map from mapi because we don't care about index
        |> Array.map (fun (_, w) -> List.toArray w.Segments)


    // iterates over segments to find horizontal segments
    for w1 in 0 .. segs.Length - 1 do
        for h in segs[w1] do
            if h.Dir = Horizontal then

                // Create a new variable for the potential jumps
                let mutable jumps: (float * SegmentId) list = []
                
                // Condition to only check moved segments
                if not (Array.contains h.HostId wiresWithNoJumpsA) then

                    // Starts iterating over the 2D array again to check if the horizontal segment intersects with any vertical segments
                    for w2 in 0 .. segs.Length - 1 do
                        for v in segs[w2] do

                            // Condition to only check moved segments
                            if not (Array.contains v.HostId wiresWithNoJumpsA) then
                                
                                // everything inside the inner loop should be very highly optimised
                                // it is executed n^2 time where n is the number of segments (maybe 5000)
                                // the abs here are because segment coordinates my be negated to indicate manual routing

                                // Fancy if to check for vertical segment
                                match v.Dir with
                                | Vertical ->
                                    // Intersection checking
                                    let x, x1, x2 = abs v.Start.X, abs h.Start.X, abs h.End.X
                                    let y, y1, y2 = abs h.Start.Y, abs v.Start.Y, abs v.End.Y
                                    let xhi, xlo = max x1 x2, min x1 x2
                                    let yhi, ylo = max y1 y2, min y1 y2
                                    //printfn $"{[xlo;x;xhi]}, {[ylo;y;yhi]}"
                                    if x < xhi - 5.0 && x > xlo + 5.0 && y < yhi - 5.0 && y > ylo + 5.0 then
                                        //printfn "found a jump!"
                                        jumps <- (x, v.Id) :: jumps
                                // Empty tuple if not intersecting or not vertical
                                | _ -> ()

                // compare jumps with what segment now has, and change newWX if need be
                // note that if no change is needed we do not update WX
                // simple cases are done without sort for speed, proably not necessary!
                // The jump list is sorted in model to enable easier rendering of segments

                // Match removed
                let sortedNewJumps = List.sort jumps
                // bugfixed
                if sortedNewJumps <> h.JumpCoordinateList then setJumpsForSegmentInWire h.HostId h.Index sortedNewJumps else ()

    { model with WX = newWX }


// Profiler
// One of the inputs is unused but the function may be useful for debugging purposes if performance becomes an issue
/// Calculates all wirejumps and prints the time it takes
let updateWireSegmentJumps (wireList: list<ConnectionId>) (wModel: Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    let model = makeAllJumps [] wModel
    TimeHelpers.instrumentTime "UpdateJumps" startT
    model

/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    // Separate moved wires into different classes
    let (inputWires, outputWires, fullyConnected) = filterWiresByCompMoved model compIdList

    let filterfunction (cId,wire) =

        if List.contains cId fullyConnected
        then (cId, moveWire wire diff)

        // If the wire is an input, reroute the input side of the wire and update
        elif List.contains cId inputWires
        then (cId, updateWire model wire true)

        // If the wire is an input, don't reroute the input side of the wire and update
        elif List.contains cId outputWires
        then (cId, updateWire model wire false)

        // If the wire was not moved don't do anything to it
        else (cId, wire)

    let newWires = 
        model.WX
        |> Map.toList
        |> List.map filterfunction
        |> Map.ofList

        
    {model with WX = newWires}

///
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd

    | UpdateWires (componentIdList, diff) -> 
        let newModel = 
            updateWires model componentIdList diff
            |> findIntersectionCoordinates [||]
            
        newModel, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let wireId = ConnectionId(JSHelpers.uuid())
        
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = []
            }
            |> autorouteWire model
        
        let wireAddedMap = Map.add newWire.Id newWire model.WX
        let newModel = 
            updateWireSegmentJumps [wireId] {model with WX = wireAddedMap}
            |> findIntersectionCoordinates [||] // Ignore wires not implemented so the input list is empty

        newModel, Cmd.ofMsg BusWidths

    | BusWidths ->
        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ wire  =
                let width =
                    match connWidths[wire.Id] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor = if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with Width = width ; Color = newColor} )

            let addSymbolWidthFolder (m: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports[match wire.InputPort with InputPortId ip -> PortId ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = m[symId]

                    match symbol.Component.Type with
                    | SplitWire n ->
                        match inPort.PortNumber with 
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym m)
                    | MergeWires ->
                        match inPort.PortNumber with 
                        | Some 0 -> 
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 -> 
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> m

            let newWX = ((Map.empty, model.WX) ||> Map.fold addWireWidthFolder)

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWX) ||> Map.fold addSymbolWidthFolder

            { model with 
                WX = newWX; Notifications = None ; 
                ErrorWires=[]; 
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}}, Cmd.none    
        


        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model )
        
        
        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with 
                    Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)
    
    | CopyWires (connIds : list<ConnectionId>) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.WX
        { model with CopiedWX = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) -> 
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}
                    else if List.contains id model.ErrorWires then 
                        {wire with Color = HighLightColor.DarkSlateGrey}
                    else wire
                ) 
        
        {model with WX = newWX ; ErrorWires = connectionIds}, Cmd.none

    | SelectWires (connectionIds : list<ConnectionId>) -> //selects all wires in connectionIds, and also deselects all other wires
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id model.ErrorWires then
                        if List.contains id connectionIds then 
                            {wire with Color = HighLightColor.Brown} 
                        else 
                            {wire with Color = HighLightColor.Red}
                    else if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Purple} 
                    else
                        {wire with Color = HighLightColor.DarkSlateGrey} 
                ) 
        
        {model with WX = newWX}, Cmd.none

    | DeleteWires (connectionIds : list<ConnectionId>) -> 
        let newModel = makeAllJumps (connectionIds) (model)
        let newWX =
             newModel.WX
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        {newModel with WX = newWX}, Cmd.ofMsg BusWidths

    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with
        | MouseOp.Down ->
            // TODO I remvoed a function from section 2 and inlined it here
            // It was a one liner after some changes I made
            // If you don't agree let me know
            // Ignacio Bricchi
            let segId = (getClosestSegment model connId mMsg.Pos).Id
            {model with SelectedSegment = segId }, Cmd.none
        | Drag ->
            let segId = model.SelectedSegment
            let rec getSeg (segList: list<Segment>) = 
                match segList with
                | h::t -> if h.Id = segId then h else getSeg t
                | _ -> failwithf "segment Id not found in segment list"
            let seg = getSeg model.WX[connId].Segments
            if seg.Draggable then
                let distanceToMove = 
                    match seg.Dir with
                    | Horizontal -> mMsg.Pos.Y - abs seg.Start.Y
                    | Vertical -> mMsg.Pos.X - abs seg.Start.X

                let newWire = moveSegment seg distanceToMove model
                let newWX = Map.add seg.HostId newWire model.WX
 
                {model with WX = newWX}, Cmd.none
            else
                model, Cmd.none
        
        | _ -> model, Cmd.none


    | ColorWires (connIds, color) -> // Just Changes the colour of the wires, Sheet calls pasteWires before this
        let newWires =
            (List.fold (fun prevWires cId -> 
                let oldWireOpt = Map.tryFind cId model.WX
                match oldWireOpt with
                | None -> 
                    printfn "BusWire error: expected wire in ColorWires does not exist"
                    prevWires
                | Some oldWire ->
                    Map.add cId { oldWire with Color = color } prevWires) model.WX connIds)
        { model with WX = newWires }, Cmd.none
    
    | ResetJumps connIds ->
        //printfn $"resetting jumps on {connIds.Length} wires"
        
        let newModel =
            model
            |> makeAllJumps connIds
        
        {newModel with segmentSplitPos = []}, Cmd.none
    
    | MakeJumps connIds ->
        printfn $"making jumps on {connIds.Length} wires"

        let newModel =
            model
            |> updateWireSegmentJumps connIds
            |> findIntersectionCoordinates [||] // Ignored wires not implemented therefore ignored wires list is empty      
            
        newModel, Cmd.none
    
    | ResetModel -> 
        { model with WX = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none
    
    | LoadConnections conns -> // we assume components (and hence ports) are loaded before connections
        let posMatchesVertex (pos:XYPos) (vertex: Vertex) =
            printfn "Pos: %A -> Vertex: %A" pos vertex
            let epsilon = 0.00001
            abs (pos.X - vertex.X) < epsilon &&
            abs (pos.Y - vertex.Y) < epsilon
            |> (fun b -> if not b then printf $"Bad wire endpoint match on {pos} {vertex}"; b else b)
        let newWX =
            conns 
            |> List.map (
                fun conn ->
                    let inputId = InputPortId conn.Target.Id
                    let outputId = OutputPortId conn.Source.Id
                    let connId = ConnectionId conn.Id
                    let segments = issieVerticesToSegments connId conn.Vertices
                    let makeWirePosMatchSymbol inOut (wire:Wire) =
                        match inOut with
                        | true -> posMatchesVertex 
                                    (Symbol.getPortPosition model.Symbol (inputId |> portIdFromInputOutputPortId))
                                    (List.last conn.Vertices)
                        | false -> posMatchesVertex 
                                    (Symbol.getPortPosition model.Symbol (outputId |> portIdFromInputOutputPortId))
                                    (List.head conn.Vertices)
                        |> (fun b -> 
                            if b then
                                // printfn "Matched"
                                wire 
                            else
                                // printfn "Did not match"
                                let getS (connId:string) = 
                                    Map.tryFind (PortId connId) model.Symbol.Ports
                                    |> Option.map (fun port -> port.HostId)
                                    |> Option.bind (fun symId -> Map.tryFind (ComponentId symId) model.Symbol.Symbols)
                                    |> Option.map (fun sym -> sym.Component.Label)
                                printfn $"Updating loaded wire from {getS conn.Source.Id}->{getS conn.Target.Id} of wire "
                                updateWire model wire inOut)
                        
                    connId,
                    {
                        Id = ConnectionId conn.Id
                        InputPort = inputId
                        OutputPort = outputId
                        Color = HighLightColor.DarkSlateGrey
                        Width = 1
                        Segments = segments
                    }
                    |> makeWirePosMatchSymbol true
                    |> makeWirePosMatchSymbol false
                )
            |> Map.ofList
        
        let connIds =
            conns
            |> List.map (fun conn -> ConnectionId conn.Id)
            
        { model with WX = newWX }, Cmd.ofMsg (MakeJumps connIds)

    | WireView v ->
        { model with WireDisplay = v}, Cmd.none

//---------------Other interface functions--------------------//

///
let wireIntersectsBoundingBox (w : Wire) (bb : BoundingBox) =
    // TODO I changed the name and signature of segmentIntersectsBoundingBoxCoordinate to
    // segmentIntersectsBoundingBox because no where in the code did it ever use the computed coordinate
    // Now it just returns a boolean instead of a booleand and XYPos
    // I updated it here to reflect that change
    // If you don't agree let me know
    // Ignacio Bricchi
    let boolList = List.map (fun seg -> segmentIntersectsBoundingBox seg bb) w.Segments
    List.contains true boolList

///
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> = 
    wModel.WX
    |> Map.map (fun id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun id boolVal -> boolVal)
    |> Map.toList
    |> List.map (fun (id,bool) -> id)

///searches if the position of the cursor is on a wire in a model
///Where n is 5 pixels adjusted for top level zoom
let getWireIfClicked (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.X = pos.X - n; Y = pos.Y - n; H = n*2.; W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires

///
let pasteWires (wModel : Model) (newCompIds : list<ComponentId>) : (Model * list<ConnectionId>) =
    let oldCompIds = Symbol.getCopiedSymbolIds wModel.Symbol
    
    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(JSHelpers.uuid())
    
            match Symbol.tryFindCorrespondingPastedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->

                // NOTE: Fixed by Nicholas, please refactor!
                let portOnePos, portTwoPos = (Symbol.getPortPosition (wModel.Symbol) (newInputPort |> portIdFromInputOutputPortId), Symbol.getPortPosition wModel.Symbol (newOutputPort |> portIdFromInputOutputPortId))
                [
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId (string newInputPort);
                            OutputPort = OutputPortId (string newOutputPort);
                    }
                    |> autorouteWire wModel
                ]
            | None -> []
        
        wModel.CopiedWX
        |> Map.toList
        |> List.map snd
        |> List.collect createNewWire
        |> List.map (fun wire -> wire.Id, wire)
        |> Map.ofList
    
    let newWireMap = Map.fold ( fun acc newKey newVal -> Map.add newKey newVal acc ) pastedWires wModel.WX
    let pastedConnIds =
        pastedWires
        |> Map.toList
        |> List.map fst
        
    { wModel with WX = newWireMap }, pastedConnIds

///
let getPortIdsOfWires (model: Model) (connIds: ConnectionId list) : (InputPortId list * OutputPortId list) =
    (([], []), connIds)
    ||> List.fold (fun (inputPorts, outputPorts) connId ->
            (model.WX[connId].InputPort :: inputPorts, model.WX[connId].OutputPort :: outputPorts))