module VerilogTypesNew

open VerilogTypes
/////////////////// Types to store the AST internally

// NumberT

// Only one module in one file?


type ItemDU =
    | IOItem of IOItemT
    | ParamDecl of ParameterItemT
    | ContinuousAssign of ContinuousAssignT
    | AlwaysConstruct of AlwaysConstructT

type StatementDU =
    | NonBlockingAssign of NonBlockingAssignT
    | BlockingAssign of BlockingAssignT
    | SeqBlock of SeqBlockT
    | Case of CaseStatementT
    | Conditional of ConditionalT

// should I add everything in here?
type ASTNode = 
    | IOItem of IOItemT
    | ParamDecl of ParameterItemT
    | ContinuousAssign of ContinuousAssignT
    | AlwaysConstruct of AlwaysConstructT
    | Statement of StatementT
    | NonBlockingAssign of NonBlockingAssignT
    | BlockingAssign of BlockingAssignT
    | SeqBlock of SeqBlockT
    | Case of CaseStatementT
    | CaseItem of CaseItemT
    | Conditional of ConditionalT
    | IfStatement of IfStatementT
    | Assignment of AssignmentT
    | AssignmentLHS of AssignmentLHST
    | Expression of ExpressionT
    | Primary of PrimaryT
    | ParameterItem of ParameterItemT
    | Parameter of ParameterT
    | Range of RangeT
    | Number of NumberT
    | Item of ItemT
    // | ModuleName of ModuleNameT
    // | Identifier of IdentifierT
type Module = {AST: ASTNode;}


///////////////////// Error handling helpers /////////////////////////

/// converts StatementT into StatementDU
let getAlwaysStatement (s: StatementT) : StatementDU =
    match s.BlockingAssign, s.NonBlockingAssign, s.CaseStatement, s.Conditional, s.SeqBlock with
    | Some blocking, None, None, None, None -> StatementDU.BlockingAssign(blocking)
    | None, Some nonblocking, None, None, None -> StatementDU.NonBlockingAssign(nonblocking)
    | None, None, Some case, None, None -> StatementDU.Case(case)
    | None, None, None, Some cond, None -> StatementDU.Conditional(cond)
    | None, None, None, None, Some seqBlock -> StatementDU.SeqBlock(seqBlock)
    | _ -> failwithf "Should not happen!"
//maybe we can combine these two, or do smth smarter
let statementToNode (statement:StatementDU) : ASTNode =
    match statement with
    | StatementDU.BlockingAssign blocking -> BlockingAssign(blocking)
    | StatementDU.NonBlockingAssign nonblocking -> NonBlockingAssign(nonblocking)
    | StatementDU.Case case -> Case(case)
    | StatementDU.Conditional cond -> Conditional(cond)
    | StatementDU.SeqBlock seqBlock -> SeqBlock(seqBlock)


let getItem (item: ItemT)  =
    match item.IODecl, item.ParamDecl, item.Statement, item.AlwaysConstruct with
    | Some ioDecl, None, None, None -> IOItem ioDecl
    | None, Some paramDecl, None, None -> ParamDecl paramDecl
    | None, None, Some contAssign, None -> ContinuousAssign contAssign
    | None, None, None, Some always -> AlwaysConstruct always
    | _ -> failwithf "Should not happen"

/// Recursively folds over an ASTNode, calling folder at every level. Only explores parts where there are multiple possibilities within a Node
let rec foldAST folder state (node:ASTNode) =
    let state' = folder state node
    match node with
    | Item item -> 
        foldAST folder state' (getItem item)
    | AlwaysConstruct always ->
        foldAST folder state' (Statement(always.Statement))
    | Statement statement -> 
        statement
        |> getAlwaysStatement
        |> statementToNode
        |> foldAST folder state'
    | SeqBlock seqBlock ->
        seqBlock.Statements
        |> Array.map (fun s -> Statement(s))
        |> Array.fold (foldAST folder) state'
    | Case case ->
        let tmpState = 
            case.CaseItems
            |> Array.map (fun item -> CaseItem(item))
            |> Array.fold (foldAST folder) state'
        match case.Default with
        | Some stmt -> List.fold folder tmpState [Statement(stmt)]
        | _ -> tmpState
    | CaseItem caseItem -> foldAST (foldAST folder) state' (Statement(caseItem.Statement))
    | Conditional cond ->
        let tmpState =
            cond.IfStatements
            |> Array.map (fun stmt -> IfStatement(stmt))
            |> Array.fold (foldAST folder) state'
        match cond.ElseStatement with
        | Some elseStmt -> List.fold (foldAST folder) tmpState [Statement(elseStmt)]
        | _ -> tmpState
    | _ -> failwithf "Should not happen"


/// returns a list of all assignments in the AST
let rec getAllAssignments (assignments) (node: ASTNode) =
    match node with
    | Item item ->
        match item.Statement, item.AlwaysConstruct with
        | Some cont_assign, _ -> assignments @ [cont_assign.Assignment] 
        | _, Some always_constr -> getAllAssignments assignments (Statement(always_constr.Statement))
        | _ -> assignments
    | Statement statement ->
        match getAlwaysStatement statement with
        | StatementDU.BlockingAssign blocking -> assignments @ [blocking.Assignment]
        | StatementDU.NonBlockingAssign nonblocking -> assignments @ [nonblocking.Assignment]
        | StatementDU.SeqBlock seqBlock -> 
            seqBlock.Statements
            |> Array.map (fun s -> Statement(s))
            |> Array.fold getAllAssignments assignments
        | StatementDU.Conditional cond ->
            let ifAssignments =
                cond.IfStatements
                |> Array.map (fun ifstmt -> Statement(ifstmt.Statement))
                |> Array.fold getAllAssignments assignments
            match cond.ElseStatement with
            | Some statement -> 
                getAllAssignments ifAssignments (Statement(statement))
            | _ -> ifAssignments
        | StatementDU.Case case -> 
            let caseAssignments =
                case.CaseItems
                |> Array.map (fun caseitem -> Statement(caseitem.Statement))
                |> Array.fold getAllAssignments assignments 
            match case.Default with
            | Some statement -> 
                getAllAssignments caseAssignments (Statement(statement))
            | _ -> caseAssignments                
    | _ -> assignments

// returns all nonassignment expressions
let rec getAllExpressions (expressions: List<ExpressionT>) (node: ASTNode) =
    match node with
    | Item item ->
        match item.AlwaysConstruct with
        | Some alwaysConstr -> 
            getAllExpressions expressions (Statement(alwaysConstr.Statement))
        | _ -> expressions
    | Statement s ->
        match getAlwaysStatement s with
        | StatementDU.SeqBlock seqBlock -> 
            seqBlock.Statements
            |> Array.map (fun s -> Statement(s))
            |> Array.fold getAllExpressions expressions
        | StatementDU.Conditional cond ->
            let ifConditions =
                cond.IfStatements
                |> Array.toList
                |> List.map (fun ifstmt -> ifstmt.Condition)
            // need to look through the statement as well
            let ifExpressions =
                cond.IfStatements
                |> Array.map (fun ifstmt -> Statement(ifstmt.Statement))
                |> Array.fold getAllExpressions []
            let expr = expressions @ ifConditions @ ifExpressions
            match cond.ElseStatement with
            | Some statement -> 
                getAllExpressions expr (Statement(statement))
            | _ -> expr
        | StatementDU.Case case ->
            let caseItemExpr =
                case.CaseItems
                |> Array.collect (fun caseItem -> caseItem.Expressions)
                |> Array.toList
            let caseItemExpr' =
                case.CaseItems
                |> Array.map (fun caseItem -> Statement(caseItem.Statement))
                |> Array.fold getAllExpressions (expressions @ caseItemExpr)
            match case.Default with
            | Some statement -> 
                getAllExpressions caseItemExpr' (Statement(statement))
            | _ -> caseItemExpr'
        | _ -> expressions
    | _ -> expressions