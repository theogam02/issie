# Code review meeting comments

## TC code review

Generally:
- Very good commenting, helps in understanding fast how the code works
- In general good naming (see below)
- Good separation of code into functions + good code seperation within functions (newlines, matches)

Points to discuss:

- It is a bit unclear what Display-DisplayLine does (type SnapData)
- toCoord function -> a bit vague name -> not clear what it does (maybe getXorYCoord)
- getOtherWireEndsPorts -> hard to read name (although it does explain what the function does) -> maybe getConnectedPorts or getPortsConnectedTo (~~pId~~ -> portId)  
- SymbolMatch: Can't think of a more suitable name, but definitely needs commenting as it's hard to read because of the colouring/numbers apearing
  - Maybe more readable: (??, not sure)
    ```
    let symbolMatch (symbol: SymbolT.Symbol) =
        match symbol.Component.Type with
        | Input _ | Output _| IOLabel 
            -> Input 0
        
        | BusCompare _ | BusSelection _ 
            -> BusCompare (0,0u)
        
        | Constant _ | Constant1 _ 
            -> Constant (0, 0L)
        
        | NbitsAdder _ | NbitsXor _ 
            -> NbitsAdder 0
        
        | MergeWires | SplitWire _ 
            -> MergeWires
        
        | DFF | DFFE | Register _ | RegisterE _ 
            -> DFF
        
        | AsyncROM x | ROM x | RAM x 
            -> RAM x 
        
        | AsyncROM1 x | ROM1 x | RAM1 x | AsyncRAM1 x 
            -> ROM1 x
        
        | x -> x
    ```

<br><br>

## JZ code review

- setClkCycle
  - clearer if:
    ```
    if newClkCycle' < 0 then failwithf "..."

    let (currClkCycle, startCycle, isEmpty)
        if (newClkCycle' <= endCycle wsModel) then
            if (newClkCycle < wsModel.StartCycle) then (X,Y,Z)
            else (X,Y,Z)
        else (X,Y,Z)

    dispatch <| InitiateWaveSimulation
            {wsModel with
                CurrClkCycle = currClkCycle
                StartCycle = startCycle
                ClkCycleBoxIsEmpty = isEmpty
            }

    ```

- valueRows, clkCycleNumberRow -> can't understand what these functions do exactly -> better commenting/naming?
- Everything else is just React Elements -> not much to say...

<br><br>

## AD Code Review 

- Good commenting
- As a person who sees the simulator code for the first time, all these very short variable names make it harder to understand the code as you constantly need to check around to understand what they stand for (acc, rem, cid, w,n, ti, ...)
  - Longer names would make the code messier but in my opinion much easier to understand

- Lots of repeating code in the last two functions
  - Could it be one function with an extra parameter?
