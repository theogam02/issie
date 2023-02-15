# To compile the .ne file to .js file:
#   1. Install the nearley compiler
#   2. Run: "npx nearleyc VerilogGrammar.ne -o VerilogGrammar.js"

##############################################  SOURCE TEXT  ##################################################

PROGRAM -> MODULE {%function(d) {return {Type: "program", Module: d[0]};} %}
# _ for optional whitespace, __ for obligatory whitespace
MODULE 
    -> _ "module" __ NAME_OF_MODULE _ "(" _ LIST_OF_PORTS:? _ ")" _ ";" _ MODULE_ITEMS _ endmodule _ {%function(d) {return {Type: "module_old", ModuleName: d[3], PortList: d[7], ModuleItems: d[13]};} %}
    | _ "module" __ NAME_OF_MODULE _ "(" _ IO_ITEMS:? _ ")" _ ";" _ NON_PORT_MODULE_ITEMS _ endmodule _ {%function(d) {return {Type: "module_new", ModuleName: d[3], IOItems: d[7], ModuleItems: d[13]};} %}

NAME_OF_MODULE -> IDENTIFIER {% id %}
 
LIST_OF_PORTS
    -> PORT _ "," _ LIST_OF_PORTS {%function(d, l, reject) {return {Type: "port_list", Head: d[0], Tail: d[4], Location: l};} %}
    | PORT {% function(d,l,reject) {return {Type: "port_list", Head: d[0], Tail: null, Location: l};}  %}

PORT -> IDENTIFIER {%function(d) {return {Type: "port", Port: d[0]};} %}

MODULE_ITEMS -> MODULE_ITEM:* {%function(d) {return {Type: "module_items", ItemList: d[0]};} %}

NON_PORT_MODULE_ITEMS -> NON_PORT_MODULE_ITEM:* {%function(d) {return {Type: "module_items", ItemList: d[0]};} %}

MODULE_ITEM
    -> INPUT_DECL {%function(d,l, reject) {return {Type: "item", ItemType: "input_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: l};} %}
    | OUTPUT_DECL ";" _ {%function(d,l, reject) {return {Type: "item", ItemType: "output_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: l};} %}
    | NON_PORT_MODULE_ITEM {% id %} #?

NON_PORT_MODULE_ITEM
   -> CONTINUOUS_ASSIGNMENT _ {%function(d,l, reject) {return {Type: "item", ItemType: "statement", IODecl: null, ParamDecl: null, Statement: d[0], AlwaysConstruct: null, Location: l};} %}
    | ALWAYS_CONSTRUCT {%function(d,l, reject) {return {Type: "item", ItemType: "always_construct", IODecl: null, ParamDecl: null, Statement: null, AlwaysConstruct: d[0], Location: l};} %}
    | "wire" __ EVERYTHING {%function(d,l, reject) {return {Type: "WIRE-DECL", ItemType: d[0], IODecl: null, ParamDecl: null, Statement: null, AlwaysConstruct: null, Location: l};} %}
    | "reg" __ EVERYTHING {%function(d,l, reject) {return {Type: "NO-COMB", ItemType: d[0], IODecl: null, ParamDecl: null, Statement: null,  AlwaysConstruct: null, Location: l};} %}
    # | "always" EVERYTHING {%function(d,l, reject) {return {Type: "NO-COMB", ItemType: d[0], IODecl: null, ParamDecl: null, Statement: null, Location: l};} %}
    #| "always_comb" EVERYTHING {%function(d,l, reject) {return {Type: "NO-COMB", ItemType: d[0], IODecl: null, ParamDecl: null, Statement: null, Location: l};} %}
    | "initial" EVERYTHING {%function(d,l, reject) {return {Type: "NO-COMB", ItemType: d[0], IODecl: null, ParamDecl: null, Statement: null,  AlwaysConstruct: null, Location: l};} %}
    | "case" EVERYTHING {%function(d,l, reject) {return {Type: "NO-CASE", ItemType: d[0], IODecl: null, ParamDecl: null, Statement: null,  AlwaysConstruct: null, Location: l};} %}

IO_ITEMS 
    -> IO_ITEM _ "," _ IO_ITEMS {%function(d) {return {Type: "io_items", Head: d[0], Tail: d[4]};} %}
    | IO_ITEM {%function(d) {return {Type: "io_items", Head: d[0], Tail: null};} %}

IO_ITEM
    -> INPUT_DECL _ {%function(d,l, reject) {return {Type: "item", ItemType: "input_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: l};} %}
    | OUTPUT_DECL _ {%function(d,l, reject) {return {Type: "item", ItemType: "output_decl", IODecl: d[0], ParamDecl: null, Statement: null, Location: l};} %}

STATEMENTS -> STATEMENT_ITEM:* {%function(d) {return {Type: "module_items", ItemList: d[0]};} %}

STATEMENT_ITEM -> 
    STATEMENT _ {%function(d,l, reject) {return {Type: "item", ItemType: "statement", IODecl: null, ParamDecl: null, Statement: d[0], Location: l};} %}
############################################    DECLARATIONS    ###############################################


INPUT_DECL -> input __ RANGE:? _ LIST_OF_VARIABLES _  {%function(d) {return {Type: "declaration", DeclarationType: "input", Range: d[2], Variables: d[4]};} %}

OUTPUT_DECL -> output __ RANGE:? _ LIST_OF_VARIABLES _ {%function(d) {return {Type: "declaration", DeclarationType: "output", Range: d[2], Variables: d[4]};} %}

LIST_OF_VARIABLES
    -> NAME_OF_VARIABLE _ "," _ LIST_OF_VARIABLES {%function(d) {return {Type: "variable_list", Head: d[0], Tail: d[4]};} %}
    | NAME_OF_VARIABLE {% function(d) {return {Type: "variable_list", Head: d[0], Tail: null};}  %}

NAME_OF_VARIABLE -> IDENTIFIER {%function(d) {return {Type: "variable", Name: d[0]};} %}

RANGE -> "[" UNSIGNED_NUMBER ":" UNSIGNED_NUMBER "]" {%function(d,l,reject) {return {Type: "range", Start: d[1], End: d[3], Location: l};} %}


######################################     BEHAVIORAL STATEMENTS    #############################################

### PROCEDURAL BLOCKS AND ASSIGNMENTS

#initial_construct -> "initial" statement_or_null #maybe dont need it
ALWAYS_CONSTRUCT -> ALWAYS_KEYWORD __ STATEMENT {%function(d) {return {Type: "always_construct", AlwaysType: d[0], Statement: d[2]};} %}

ALWAYS_KEYWORD -> "always" {% id %} | "always_comb" {% id %} | "always_latch" {% id %} | "always_ff" {% id %} #remove latch and maybe basic
#final_construct -> "final" function_statement # maybe dont need it

BLOCKING_ASSIGNMENT ->
    # VARIABLE_LVALUE "=" EXPRESSION {%function(d) {return {Type: "blocking_assignment", LHS: d[0]}, RHS: d[2];} %}# dont need delay DELAY_OR_EVENT_CONTROL
    #| NONRANGE_VARIABLE_LVALUE "=" DYNAMIC_ARRAY_NEW #probs dont need it
    # | [ implicit_class_handle . | class_scope | package_scope ] hierarchical_variable_identifier #figure out what this is
    #select = class_new
    OPERATOR_ASSIGNMENT {%function(d) {return {Type: "blocking_assignment", Operator: d[0].Operator, Assignment: d[0].Assignment};} %}

OPERATOR_ASSIGNMENT -> VARIABLE_LVALUE _ ASSIGNMENT_OPERATOR _ EXPRESSION 
    {%function(d) {return {Type: "operator_assignment", Operator: d[2], Assignment: {Type: "assignment", LHS: d[0], RHS: d[4]}};} %}

ASSIGNMENT_OPERATOR ->
    "=" {% id %}| "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "<<<=" | ">>>=" {% id %}

NONBLOCKING_ASSIGNMENT ->
    VARIABLE_LVALUE _ "<=" _ EXPRESSION #don't need delay or event control [ delay_or_event_control ]
    {%function(d) {return {Type: "nonblocking_assignment", Assignment: {Type: "assign", LHS: d[0], RHS: d[4]}};} %}

#PROCEDURAL_CONTINUOUS_ASSIGNMENT ->
    #"assign" VARIABLE_ASSIGNMENT
    #| deassign variable_lvalue
    #| force variable_assignment
    #| force net_assignment
    #| release variable_lvalue
    #| release net_lvalue

#VARIABLE_ASSIGNMENT -> VARIABLE_LVALUE "=" EXPRESSION

VARIABLE_LVALUE -> L_VALUE {% id %}

#need multiple statements not just one
SEQ_BLOCK
    -> "begin" __ SEQ_BLOCK_STMTS _ "end" __ {% function(d) {return{Type: "seq_block", Statements: d[2]}; } %}# not sure if this is correct

SEQ_BLOCK_STMTS -> STATEMENT:+ {% function(d) {return d[0]}%}

CONDITIONAL_STATEMENT ->
    IF ELSE_IF:* ELSE:? {%function(d) {if_statements=[d[0]].concat(d[1]); return {Type: "cond_stmt", IfStatements: if_statements, ElseStatement: d[2]}}%}

# this might be ambiguous grammar? I want to have it this way because code gen should be easier maybe
IF -> "if" _ "(" EXPRESSION ")" _ STATEMENT {% function(d) {return {Type: "ifstmt", Condition: d[3], Statement: d[6]}; } %}
ELSE_IF -> "else" __ "if" _ "(" EXPRESSION ")" _ STATEMENT {% function(d) {return {Condition: d[5], Statement: d[8]}; } %}
ELSE -> "else" __ STATEMENT {% function(d) {return d[2]; } %}

STATEMENT
    -> NONBLOCKING_ASSIGNMENT _ ";" _ {%function(d) {return {Type: "statement", StatementType: "nonblocking_assignment", NonBlockingAssign: d[0], BlockingAssign: null, SeqBlock: null, Conditional: null, CaseStatement: null};} %}
    | BLOCKING_ASSIGNMENT _ ";" _ {%function(d) {return {Type: "statement", StatementType: "blocking_assignment", NonBlockingAssign: null, BlockingAssign: d[0], SeqBlock: null, Conditional: null,  CaseStatement: null};} %}
    | SEQ_BLOCK _ {%function(d) {return {Type: "statement", StatementType: "seq_block", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: d[0], Conditional: null,  CaseStatement: null};} %} #change to statements?
    | CONDITIONAL_STATEMENT _ {%function(d){return {Type: "statement", StatementType: "conditional", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: d[0],  CaseStatement: null};}%}
    | CASE_STATEMENT _ {%function(d){return {Type: "statement", StatementType: "case_stmt", NonBlockingAssign: null, BlockingAssign: null, SeqBlock: null, Conditional: null,  CaseStatement: d[0]};}%}


################# CASE STATEMENTS ###################
CASE_STATEMENT # probs only care about first one
    -> "case" _ "(" _ EXPRESSION _ ")" _ (CASE_ITEM {% id %}):+ _ DEFAULT:? "endcase" __ {%function(d) { return {Type: "case_stmt", Expression: d[4], CaseItems: d[8], Default: d[10]};}%}
    #| CASE_KEYWORD (case_expression ) "matches" case_pattern_item { case_pattern_item } endcase
    #|  "case" ( case_expression ) "inside"
    #   case_inside_item { case_inside_item } endcase


# for clarity default should be last
DEFAULT
    -> "default" _ ":" _ STATEMENT _ {% function(d){return d[4];}%}
CASE_ITEM
    -> EXPRESSION  _ ("," _ EXPRESSION {%function(d){return d[2];}%}):* _ ":" _ STATEMENT _
        {% function(d) {expr = [d[0]].concat(d[2]); return {Type: "case_item", Expressions: expr, Statement: d[6]};}%}
    #| default [ : ] statement_or_null

# case_pattern_item ::=
#     pattern [ &&& expression ] : statement_or_null
#     | default [ : ] statement_or_null
# case_inside_item ::=
#     open_range_list : statement_or_null
#     | default [ : ] statement_or_null


# PROCEDURAL_TIMING_CONTROL_STATEMENT
#     -> PROCEDURAL_TIMING_CONTROL _ STATEMENT {% %}

# EDGE_IDENTIFIER -> "posedge" | "negedge" | "edge"

# EVENT_EXPRESSION
#     -> EDGE_IDENTIFIER:? EXPRESSION ("iff" __ EXPRESSION):?
#     #| sequence_instance [ iff expression ] # check what this is!
#     | EVENT_EXPRESSION _ "or" _ EVENT_EXPRESSION
#     | EVENT_EXPRESSION _ "," _ EVENT_EXPRESSION
#     | "(" _ EVENT_EXPRESSION _ ")"

# PROCEDURAL_TIMING_CONTROL
#     -> "@" IDENTIFIER  #hierarchical_event_identifier
#     | "@" _ "(" EVENT_EXPRESSION ")"
#     | "@" _ "*"
#     | "@" _ "(" _ "*" _ ")"
#     #| "@" ps_or_hierarchical_sequence_identifier


CONTINUOUS_ASSIGNMENT
    -> assign __ ASSIGNMENT _ ";" {%function(d) {return {Type: "statement", StatementType: "assign", Assignment: d[2]};} %}
    | wire __ WIRE_ASSIGNMENT _ ";" {%function(d) {return {Type: "statement", StatementType: "wire", Assignment: d[2]};} %}

ASSIGNMENT -> L_VALUE _ "=" _ EXPRESSION {%function(d) {return {Type: "assign", LHS: d[0], RHS: d[4]};} %}

WIRE_ASSIGNMENT -> WIRE_L_VALUE _ "=" _ EXPRESSION {%function(d) {return {Type: "wire", LHS: d[0], RHS: d[4]};} %} 

###########################################      EXPRESSIONS      ###############################################

WIRE_L_VALUE
    -> IDENTIFIER {%function(d) {return {Type: "l_value", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};} %}
    | "[" UNSIGNED_NUMBER ":" UNSIGNED_NUMBER "]" _ IDENTIFIER {%function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: d[1], BitsEnd: d[3], Primary: d[6]};} %}

L_VALUE
    -> IDENTIFIER {%function(d) {return {Type: "l_value", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};} %}
    | IDENTIFIER _ "[" UNSIGNED_NUMBER "]" {%function(d) {return {Type: "l_value", PrimaryType: "identifier_bit", BitsStart: d[3], BitsEnd: d[3], Primary: d[0]};} %}
    | IDENTIFIER _ "[" UNSIGNED_NUMBER ":" UNSIGNED_NUMBER "]" {%function(d) {return {Type: "l_value", PrimaryType: "identifier_bits", BitsStart: d[3], BitsEnd: d[5], Primary: d[0]};} %}


EXPRESSION -> CONDITIONAL {% id %}

CONDITIONAL
    -> LOGICAL_OR _ "?" _ CONDITIONAL_RESULT {%function(d) {return {Type: "conditional_cond", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | LOGICAL_OR {% id %}

CONDITIONAL_RESULT
    -> LOGICAL_OR _ ":" _ LOGICAL_OR {%function(d) {return {Type: "conditional_result", Operator:d[2], Head: d[0], Tail: d[4]};} %}

LOGICAL_OR
    -> LOGICAL_AND _ "||" _ LOGICAL_OR {%function(d) {return {Type: "logical_OR", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | LOGICAL_AND {% id %}

LOGICAL_AND
    -> BITWISE_OR _ "&&" _ LOGICAL_AND {%function(d) {return {Type: "logical_AND", Operator:d[2], Head: d[0], Tail: d[4]};} %}  
    | BITWISE_OR {% id %}

BITWISE_OR 
    -> BITWISE_XOR _ "|" _ BITWISE_OR {%function(d) {return {Type: "bitwise_OR", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | BITWISE_XOR {% id %}

BITWISE_XOR  
    -> BITWISE_AND _ XOR_XNOR_OPERATOR _ BITWISE_XOR {%function(d) {return {Type: "bitwise_XOR", Operator:d[2][0], Head: d[0], Tail: d[4]};} %}
    | BITWISE_AND {% id %}


BITWISE_AND 
    -> LOGICAL_SHIFT _ "&" _ BITWISE_AND {%function(d) {return {Type: "bitwise_AND", Operator:d[2], Head: d[0], Tail: d[4]};} %}
    | LOGICAL_SHIFT {% id %}

LOGICAL_SHIFT
    -> ADDITIVE _ SHIFT_OPERATOR _ UNSIGNED_REDUCTED {%function(d) {return {Type: "SHIFT", Operator:d[2][0], Head: d[0], Tail: d[4]};} %}
    | ADDITIVE _ SHIFT_OPERATOR _ LOGICAL_SHIFT {%function(d) {return {Type: "SHIFT", Operator:d[2][0], Head: d[0], Tail: d[4]};} %}
    | ADDITIVE {% id %}

ADDITIVE
    -> REDUCTION_OR_NEGATION _ ADDITIVE_OPERATOR _ ADDITIVE {%function(d) {return {Type: "additive", Operator:d[2][0], Head: d[0], Tail: d[4]};} %}
    | REDUCTION_OR_NEGATION {% id %}


REDUCTION_OR_NEGATION
    -> "(" _ UNARY_OPERATOR _ UNARY _ ")" {%function(d) {return {Type: "reduction", Operator:d[2][0], Unary: d[4]};} %}
    | "~" _ UNARY {%function(d) {return {Type: "negation", Operator: "~", Unary: d[2]};} %}
    | UNARY {%function(d) {return {Type: "unary", Unary: d[0]};} %}

UNARY 
    -> PRIMARY {%function(d) {return {Type: "primary", Primary: d[0], Number: null, Expression: null};} %}
    | NUMBER {%function(d) {return {Type: "number", Primary: null, Number: d[0], Expression: null};} %}
    | "(" _ BITWISE_OR _ ")" {%function(d) {return {Type: "parenthesis", Primary: null, Number: null, Expression: d[2]};} %}
    | "{" _ LIST_OF_UNARIES _ "}" {%function(d) {return {Type: "concat", Primary: null, Number: null, Expression: d[2]};} %}


LIST_OF_UNARIES
    -> EXPRESSION _ "," _ LIST_OF_UNARIES {%function(d) {return {Type: "unary_list", Head : d[0], Tail: d[4]};} %}
    | EXPRESSION {% function(d) {return {Type: "unary_list", Head: d[0], Tail: null};}  %}


#### Used for unsigned numbers (only in logical/arithmetic shifts)
UNSIGNED_REDUCTED 
    -> UNSIGNED_UNARY {%function(d) {return {Type: "unary_unsigned", Unary: d[0]};} %}

UNSIGNED_UNARY
    -> U_NUMBER {%function(d) {return {Type: "number", Primary: null, Number: d[0], Expression: null};} %}

U_NUMBER
    -> UNSIGNED_NUMBER {%function(d,l,reject) {return {Type: "number", NumberType: "decimal", Bits: null, Base: null, UnsignedNumber: d[0], AllNumber: null, Location: l};} %}

##############

ADDITIVE_OPERATOR -> "+" | "-" {%function(d) {return d;} %}

XOR_XNOR_OPERATOR -> "^" | "~^" | "^~" {%function(d) {return d;} %}

SHIFT_OPERATOR -> "<<" | ">>" | ">>>" {%function(d) {return d;} %}

UNARY_OPERATOR -> "!"  | "&" | "~&" | "|" | "~|" {%function(d) {return d[0].join('');} %}


PRIMARY
    -> IDENTIFIER {%function(d) {return {Type: "primary", PrimaryType: "identifier", BitsStart: null, BitsEnd: null, Primary: d[0]};} %}
    | IDENTIFIER _ "[" UNSIGNED_NUMBER "]" {%function(d) {return {Type: "primary", PrimaryType: "identifier_bit", BitsStart: d[3], BitsEnd: d[3], Primary: d[0]};} %}
    | IDENTIFIER _ "[" UNSIGNED_NUMBER ":" UNSIGNED_NUMBER "]" {%function(d) {return {Type: "primary", PrimaryType: "identifier_bits", BitsStart: d[3], BitsEnd: d[5], Primary: d[0]};} %}


NUMBER
    -> UNSIGNED_NUMBER "'h" ALL_NUMERIC {%function(d,l,reject) {return {Type: "number", NumberType: "all", Bits: d[0], Base: "'h", UnsignedNumber: null, AllNumber: d[2], Location: l};} %}
    | UNSIGNED_NUMBER "'b" BINARY_NUMBER {%function(d,l,reject) {return {Type: "number", NumberType: "all", Bits: d[0], Base: "'b", UnsignedNumber: null, AllNumber: d[2], Location: l};} %}
    | UNSIGNED_NUMBER "'d" UNSIGNED_NUMBER {%function(d,l,reject) {return {Type: "number", NumberType: "all", Bits: d[0], Base: "'d", UnsignedNumber: null, AllNumber: d[2], Location: l};} %}
    

UNSIGNED_NUMBER -> [0-9]:+ {%function(d) {return d[0].join('');} %}

ALL_NUMERIC -> [0-9a-fA-F]:+ {%function(d) {return d[0].join('');} %}

BINARY_NUMBER -> [0-1]:+ {%function(d) {return d[0].join('');} %}

BASE -> "'b" | "'h" {% id %}

CONCAT 
    -> EXPRESSION _ "," _ CONCAT {%function(d) {return {Type: "concatenation_list", Head: d[0], Tail: d[4]};} %}
    | EXPRESSION {% function(d) {return {Type: "concatenation_list", Head: d[0], Tail: null};}  %}


##############################################    GENERAL    #################################################

input -> "input" {% id %}
output -> "output" {% id %}
parameter -> "parameter" {% id %}
assign -> "assign" {% id %}
wire -> "wire" {% id %}
endmodule -> "endmodule" {% id %}
 
EVERYTHING -> [[a-zA-Z_0-9\s;@()]:*

IDENTIFIER -> [a-zA-Z] [a-zA-Z_0-9]:* {%
    function(d,l, reject) {
        const keywords = ["alias","and","assert","assign","assume","automatic","before","begin","bind","bins","binsof","bit","break","buf","bufif0","bufif1","byte","case","casex","casez","cell","chandle","class","clocking","cmos","config","const","constraint","context","continue","cover","covergroup","coverpoint","cross","deassign","default","defparam","design","disable","dist","do","edge","else","end","endcase","endclass","endclocking","endconfig","endfunction","endgenerate","endgroup","endinterface","endmodule","endpackage","endprimitive","endprogram","endproperty","endsequence","endspecify","endtable","endtask","enum","event","expect","export","extends","extern","final","first_match","for","force","foreach","forever","fork","forkjoin","function","generate","genvar","highz0","highz1","if","iff","ifnone","ignore_bins","illegal_bins","import","incdir","include","initial","inout","input","inside","instance","int","integer","interface","intersect","join","join_any","join_none","large","liblist","library","local","localparam","logic","longint","macromodule","matches","medium","modport","module","nand","negedge","new","nmos","nor","noshowcancelled","not","notif0","notif1","null","or","output","package","packed","parameter","pmos","posedge","primitive","priority","program","property","protected","pull0","pull1","pulldown","pullup","pulsestyle_ondetect","pulsestyle_onevent","pure","rand","randc","randcase","randsequence","rcmos","real","realtime","ref","reg","release","repeat","return","rnmos","rpmos","rtran","rtranif0","rtranif1","scalared","sequence","shortint","shortreal","showcancelled","signed","small","solve","specify","specparam","static","string","strong0","strong1","struct","super","supply0","supply1","table","tagged","task","this","throughout","time","timeprecision","timeunit","tran","tranif0","tranif1","tri","tri0","tri1","triand","trior","trireg","type","typedef","union","unique","unsigned","use","uwire","var","vectored","virtual","void","wait","wait_order","wand","weak0","weak1","while","wildcard","wire","with","within","wor","xnor","xor"]
        const name = d[0] + d[1].join('');
        if (keywords.includes(name)) {
            return reject;
        } else {
            return  {Name: name, Location: l};
        }
    }
%}

_ -> [\s]:* 
__ -> [\s]:+

