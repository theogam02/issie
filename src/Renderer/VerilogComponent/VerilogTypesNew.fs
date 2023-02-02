module VerilogTypesNew

open VerilogTypes
/////////////////// Types to store the AST internally

// NumberT

// Only one module in one file?


type Item =
    | IOItem of IOItemT
    | ParamDecl of ParemeterItemT
    | ContinuousAssign of ContinuousAssignT
    | AlwaysConstruct of AlwaysConstructT

type Statement =
    | NonBlockingAssign of NonBlockingAssignT
    | BlockingAssign of BlockingAssignT
    | SeqBlock of SeqBlockT

// should I add everything in here?
type ASTNode = 
    | IOItem of IOItemT
    | ParamDecl of ParemeterItemT
    | ContinuousAssign of ContinuousAssignT
    | AlwaysConstruct of AlwaysConstructT
    | Statement of StatementT
    | NonBlockingAssign of NonBlockingAssignT
    | BlockingAssign of BlockingAssignT
    | SeqBlock of SeqBlockT
    | Assignment of AssignmentT
    | AssignmentLHS of AssignmentLHST
    | Expression of ExpressionT
    | Primary of PrimaryT
    | ParameterItem of ParameterItemT
    | Parameter of ParameterT
    | IOItem of IOItemT
    | Range of RangeT
    | Number of NumberT
    // | ModuleName of ModuleNameT
    // | Identifier of IdentifierT
type Module = {AST: ASTNode;}