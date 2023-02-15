module VerilogTypesNew

open VerilogTypes
/////////////////// Types to store the AST internally

// NumberT

// Only one module in one file?


type Item =
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
    // | NonBlockingAssign of NonBlockingAssignT
    // | BlockingAssign of BlockingAssignT
    // | SeqBlock of SeqBlockT
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
        | BlockingAssign blocking -> assignments @ [blocking.Assignment]
        | NonBlockingAssign nonblocking -> assignments @ [nonblocking.Assignment]
        | SeqBlock seqBlock -> 
            seqBlock.Statements
            |> Array.map (fun s -> Statement(s))
            |> Array.fold getAllAssignments assignments
        | Conditional cond ->
            let ifAssignments =
                cond.IfStatements
                |> Array.map (fun ifstmt -> Statement(ifstmt.Statement))
                |> Array.fold getAllAssignments assignments
            match cond.ElseStatement with
            | Some statement -> 
                getAllAssignments ifAssignments (Statement(statement))
            | _ -> ifAssignments
        | Case case -> 
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
        | SeqBlock seqBlock -> 
            seqBlock.Statements
            |> Array.map (fun s -> Statement(s))
            |> Array.fold getAllExpressions expressions
        | Conditional cond ->
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
        | Case case ->
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