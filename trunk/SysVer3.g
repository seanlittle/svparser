grammar SysVer3;
options {
    k = 2;                       
   output=AST;
        }


@header {
package test;
import java.util.HashMap;
}

@lexer::header {package test;}

@members {
/** Map variable name to Integer object holding value */
HashMap memory = new HashMap();


public String getErrorMessage(RecognitionException e,
                              String[] tokenNames)
{
    List stack = getRuleInvocationStack(e, this.getClass().getName());
    String msg = null;
    if ( e instanceof NoViableAltException ) {
       NoViableAltException nvae = (NoViableAltException)e;
       msg = " no viable alt; token="+e.token+
          " (decision="+nvae.decisionNumber+
          " state "+nvae.stateNumber+")"+
          " decision=<<"+nvae.grammarDecisionDescription+">>";
    }
    else {
       msg = super.getErrorMessage(e, tokenNames);
    }
    return stack+" "+msg;
}
public String getTokenErrorDisplay(Token t) {
    return t.toString();
}
}
// END:override














//-----------------------------------------------------------------------------
// 1. Source Text
//-----------------------------------------------------------------------------

source_text 
scope Symbols {
 Set types;
}
translation_unit
scope Symbols;
@init {
 $Symbols::types = new HashSet();
}

:
         ( description  )* 
        EOF
    ;   

description :
        module
    |   udp 
    |   directive // TLBOZO is this 
    ;

module :
        (
            mod_type
            name_of_module  
            ( list_of_ports )?  SEMI 
            ( module_item ) *
            'endmodule' 
        )  //-> ^(mod_type name_of_module list_of_ports+ module_item+)
    ;

mod_type : ( 'module' | 'macromodule' ) ;


list_of_ports :
        LPAREN port ( COMMA port )* RPAREN -> port+
        ;

port :
	(port_expression)? 
    |  DOT name_of_port LPAREN (port_expression)? RPAREN
	;

port_expression :
        (port_reference) => port_reference
    | LBRACK port_reference ( COMMA port_reference )* RBRACK

        ;

port_reference :
        ( 
          ( name_of_variable LBRACK expression COLON ) => name_of_variable LBRACK expression COLON expression RBRACK 
        | ( name_of_variable LBRACK ) => name_of_variable LBRACK expression RBRACK
        | name_of_variable 
        ) -> name_of_variable+
        ;

name_of_port :              local_identifier ;



module_item :
        // ambiguity between net_declaration and continuous_assign,
        // but parser gets it right: keyword chosen over IDENTIFIER.
        
        parameter_declaration 
    |   output_declaration
    |   input_declaration 
    |   inout_declaration 
    |   ( net_declaration ) => net_declaration 
    |   reg_declaration 
    |   time_declaration 
    |   integer_declaration
    |   real_declaration
    |   event_declaration
    |   gate_declaration
	|   udp_instantiation
	|   (module_instantiation) => module_instantiation 
    |   parameter_override
    |   continuous_assign
    |   specify_block
    |   initial_statement
    |   always_statement
    |   task
    |   function
    |   hierarchy_declaration 
    |   directive // TLBOZO is this needed?
    ;

//----------------------------------------------------------------------------
// UDP specs
//----------------------------------------------------------------------------

udp :
        'primitive' name_of_UDP
	LPAREN name_of_variable ( COMMA name_of_variable )* RPAREN SEMI
        (udp_declaration)+
	(udp_initial_statement)?
	table_definition
        'endprimitive'
        ;


udp_declaration :
        output_declaration |
        input_declaration |
        reg_declaration
        ;

udp_initial_statement :
        'initial' output_terminal_name ASSIGN init_val SEMI
        ;

   // Use a semantic predicate to determine whether a matched NUMBER
   // is a valid special value in the given context.
   // This kludge avoids having the special values in the Literals table,
   // thus avoiding a lexical conflict.
init_val :
        '1`b0'  
    |   '1`b1' 
    |   '1`bx' 
    |    (n=NUMBER)
	{ $n.getText()=="0" || $n.getText()=="1"}?
	;

output_terminal_name :      local_identifier ;

table_definition :
        'table' table_entries 'endtable'
        ;

   // Don't try to parse table entries; just collect them.
   // There are ambiguities between edge_symbol and level_symbol,
   // and textbook Verilog examples don't seem to follow rules
   // completely. For example,
   //	"0    00    :    0;"
   // doesn't match grammar because of "00", but is frequently used.
table_entries :
	//(sequential_entry) => (sequential_entry)+ |
	//(combinational_entry)+
	(( ~(SEMI | 'endtable') )+ SEMI)*
	;
//TLBOZO missing productions for table_entries, since we are not matching this gammar:
// combinational entry
// sequential_entry
// input_list
// level_input_list 
// edge_input_list 
// edge
// state
// next_state
// OUTPUT_SYMBOL
// LEVEL_SYMBOL
// EDGE_SYMBOL


task :
        'task' name_of_task SEMI
        (tf_declaration)*
        statement_or_null
        'endtask'
        ;

name_of_task :              local_identifier ;

function :
        'function' (range_or_type)? name_of_function SEMI
        (tf_declaration)+
        statement
        'endfunction'
        ;

name_of_function :          local_identifier ;

tf_declaration :
        parameter_declaration |
        output_declaration |
        input_declaration |
        inout_declaration |
        reg_declaration |
        time_declaration |
        integer_declaration |
        real_declaration |
        event_declaration 
        ;


//----------------------------------------------------------------------------
// 2. Declarations
//----------------------------------------------------------------------------

parameter_declaration :
        'parameter' (range)? list_of_param_assignments SEMI
        ;

list_of_param_assignments :
        param_assignment ( COMMA param_assignment )*
        ;

param_assignment :
        identifier ASSIGN expression
        ;

hierarchy_declaration :
        hierarchical_identifier SEMI
    ;

input_declaration :
        'input' (range)? list_of_variables SEMI
        ;

output_declaration :
        'output' (range)? list_of_variables SEMI
        ;

inout_declaration :
        'inout' (range)? list_of_variables SEMI
        ;

net_declaration :
        ( NETTYPE (expandrange)? ) => NETTYPE (expandrange)? (delay)? list_of_assigned_variables SEMI 
        'trireg' (charge_strength)? (expandrange)? (delay)? list_of_variables SEMI
        ;

NETTYPE :
        'wire' 
|       'tri' 
|       'tri1' 
|       'supply0' 
|       'wand' 
|       'triand' 
|       'tri0' 
|       'supply1' 
|       'wor' 
|       'trior' 
|       'trireg'
        ;

expandrange :
        'scalared' range |
        'vectored' range |
        range
    ;

reg_declaration :
        'reg' (range)? list_of_register_variables SEMI
        ;

time_declaration :
        'time' list_of_register_variables SEMI
        ;

integer_declaration :
        'integer' list_of_register_variables SEMI
        ;

real_declaration :
        'real' list_of_variables SEMI
        ;

event_declaration :
        'event' name_of_event ( COMMA name_of_event )* SEMI
        ;

continuous_assign :
        'assign' (drive_strength)? (delay)? list_of_assignments SEMI |
        NETTYPE (drive_strength)? (expandrange)? (delay)?
           list_of_assignments SEMI
        ;

parameter_override :
        'defparam' list_of_param_assignments SEMI
        ;

list_of_variables :
        name_of_variable ( COMMA name_of_variable )*
        ;

name_of_variable :          local_identifier ;

list_of_register_variables :
        register_variable ( COMMA register_variable )*
        ;

register_variable :
        (  name_of_register ) => name_of_register
    |   name_of_memory LBRACK expression COLON expression RBRACK
        ;

name_of_register :          local_identifier ;

name_of_memory :            local_identifier ;

name_of_event :             local_identifier ;

charge_strength :
      (LPAREN 'small'  RPAREN ) => LPAREN 'small'  RPAREN 
    | (LPAREN 'medium' RPAREN ) => LPAREN 'medium' RPAREN 
    | LPAREN 'large'  RPAREN
        ;

drive_strength :
        ( str0 ) => str0
    |  ( str1 ) => str1
        ;

str0 :
        LPAREN strength0 COMMA strength1 RPAREN 
    ;

str1 :
        LPAREN strength1 COMMA strength0 RPAREN
    ;

strength0 :
      'supply0' 
    | 'strong0' 
    | 'pull0' 
    | 'weak0' 
	  'highz0'
        ;

strength1 :
      'supply1' 
    | 'strong1' 
    | 'pull1' 
    | 'weak1' 
	  'highz1'
        ;

range :
	(LBRACK expression COLON) =>
        LBRACK expression COLON expression RBRACK |
        LBRACK expression RBRACK
        ;

list_of_assignments :
        assignment ( COMMA assignment )*
        ;

//----------------------------------------------------------------------------
// 3. Primitive Instances
//----------------------------------------------------------------------------

gate_declaration :
        gate_type (gate_internals )+
        SEMI
	;

gate_internals :
    (drive_strength) => drive_strength
    |    (delay) => delay
    |    gate_instance ( COMMA gate_instance )* 
;



gate_type :
        'and' |
        'nand' |
        'or' |
        'nor' |
        'xor' |
        'xnor' |
        'buf' |
        'bufif0' |
        'bufif1' |
        'not' |
        'notif0' |
        'notif1' |
        'pulldown' |
        'pullup' |
        'nmos' |
        'rnmos' |
        'pmos' |
        'rpmos' |
        'cmos' |
        'rcmos' |
        'tran' |
        'rtran' |
        'tranif0' |
        'rtranif0' |
        'tranif1' |
        'rtranif1'
        ;

delay :
         (POUND NUMBER ) => POUND NUMBER 
    |   POUND local_identifier
        // TLBOZO need mintypmax epxressions too
    ;

gate_instance :
        (name_of_gate_instance)?
	LPAREN terminal ( COMMA terminal )* RPAREN
            ;

name_of_gate_instance :     local_identifier ;

udp_instantiation :
	name_of_UDP udp_internals SEMI
	;

udp_internals :
    (drive_strength) => drive_strength 
    | (delay) => delay
    | udp_instance ( COMMA udp_instance )* 
        ;

name_of_UDP :               local_identifier ;


udp_instance :
        (name_of_UDP_instance)?
	LPAREN terminal ( COMMA terminal )* RPAREN
        ;

name_of_UDP_instance :      local_identifier (range)? ;

terminal :
        ( expression ) => expression
    | local_identifier
;


//----------------------------------------------------------------------------
// 4. Module Instantiations
//----------------------------------------------------------------------------

module_instantiation :
        name_of_module (parameter_value_assignment)?
        module_instance ( COMMA module_instance )* SEMI
        ;

name_of_module :  local_identifier ;

parameter_value_assignment :
        POUND LPAREN expression ( COMMA expression )* RPAREN
        ;

module_instance :
        name_of_instance LPAREN list_of_module_connections RPAREN
        ;

name_of_instance :          local_identifier ;

list_of_module_connections :
        module_port_connection ( COMMA module_port_connection )* |
        named_port_connection ( COMMA named_port_connection )*
        ;

module_port_connection :
        expression 
	|  NULL
        ;

NULL :  // nothing - this form covers the case of an empty item in a list - for example:
	    //  (a, b, , d)
      ;


//----------------------------------------------------------------------------
// 5. Behavioral Statements
//----------------------------------------------------------------------------

initial_statement :
        'initial' statement
        ;

always_statement :
        'always' statement
        ;

statement_or_null :
        (statement) => statement |
	SEMI
        ;

statement :
        (lvalue ASSIGN) => blocking_assignment SEMI |
        (lvalue LE) => non_blocking_assignment SEMI |
        conditional_statement |
        case_statement |
        loop_statement |
        procedural_timing_control_statement |
        wait_statement |
        event_trigger |
        seq_block |
        par_block |
        task_enable |
        system_task_enable |
        disable_statement |
        procedural_continuous_assignment
        ;

assignment :
        lvalue ASSIGN expression
        ;

blocking_assignment :
        lvalue ASSIGN ( delay_or_event_control )?  expression
        ;

non_blocking_assignment :
        lvalue LE ( delay_or_event_control )?  expression
        ;

	  // 'else' clause is inherently ambiguous; ANTLR gets it right,
	  // so suppress warning.

delay_or_event_control :
        delay_control 
    |   event_control
        // TLBOZO need another entry for repeat expressions
    //| ('repeat' LPAREN expression RPAREN event_control)
        ;

case_item :
        expression ( COMMA expression )* COLON statement_or_null |
        'default' (COLON)? statement_or_null
        ;

seq_block :
        'begin'
        ( COLON name_of_block (block_declaration)* )?
        (statement)*
	'end'
	;

par_block :
        'fork'
        ( COLON name_of_block (block_declaration)* )?
        (statement)*
	'join'
        ;

name_of_block :             local_identifier ;

block_declaration :
        parameter_declaration |
        reg_declaration |
        integer_declaration |
        real_declaration |
        time_declaration |
        event_declaration 
        ;

task_enable :
        name_of_task ( LPAREN expression (COMMA (expression)?)* RPAREN )?
	SEMI
        ;

system_task_enable :
        name_of_system_task ( LPAREN expression (COMMA (expression)?)* RPAREN )?
	SEMI
        ;
name_of_system_task :
        '$' IDENTIFIER
        ;
system_identifier : local_identifier ;
       


//----------------------------------------------------------------------------
// 6. Specify Section
//----------------------------------------------------------------------------

specify_block :
        'specify' (specify_item)* 'endspecify'
        ;

specify_item :
        spec_param_declaration |
        (path_declaration) => path_declaration |
        system_timing_check
        | sdpd
        ;
spec_param_declaration :
        'specparam' list_of_specparam_assignments SEMI
        ;

list_of_specparam_assignments :
        specparam_assignment ( COMMA specparam_assignment )*
        ;

specparam_assignment :
        identifier ASSIGN expression
        ;

path_declaration :
        (simple_path_declaration) =>
	   simple_path_declaration SEMI |
        (level_sensitive_path_declaration) =>
	   level_sensitive_path_declaration SEMI |
        edge_sensitive_path_declaration SEMI
        ;

simple_path_declaration :
        (parallel_path_description) =>
	   parallel_path_description ASSIGN path_delay_value |
        full_path_descriptor ASSIGN path_delay_value
        ;

parallel_path_description :
        LPAREN specify_terminal_descriptor PPATH specify_terminal_descriptor RPAREN
        ;

full_path_descriptor :
        LPAREN list_of_path_terminals  FPATH list_of_path_terminals RPAREN
        ;

list_of_path_terminals :
        specify_terminal_descriptor ( COMMA specify_terminal_descriptor )*
        ;

specify_terminal_descriptor :
	(identifier LBRACK expression COLON) =>
           identifier LBRACK expression COLON expression RBRACK |
        (identifier LBRACK) =>
           identifier LBRACK expression RBRACK |
        identifier
        ;

path_delay_value :
        (path_delay_expression) => path_delay_expression |
        LPAREN list_of_path_delay_expressions RPAREN
        ;

list_of_path_delay_expressions :
        path_delay_expression COMMA path_delay_expression
	  ( COMMA path_delay_expression
	    ( COMMA path_delay_expression COMMA
              path_delay_expression COMMA path_delay_expression )? )?
        ; 

path_delay_expression :
        mintypmax_expression
        ;

system_timing_check :
        '$setup' LPAREN timing_check_event COMMA timing_check_event COMMA
            timing_check_limit ( COMMA notify_register )? RPAREN SEMI |
        '$hold' LPAREN timing_check_event COMMA timing_check_event COMMA
            timing_check_limit ( COMMA notify_register )? RPAREN SEMI |
        '$period' LPAREN controlled_timing_check_event COMMA
            timing_check_limit ( COMMA notify_register )? RPAREN SEMI |
        '$width' LPAREN controlled_timing_check_event COMMA
            timing_check_limit ( COMMA expression COMMA notify_register )?
	    RPAREN SEMI |
        '$skew' LPAREN timing_check_event COMMA timing_check_event COMMA 
            timing_check_limit ( COMMA notify_register )? RPAREN SEMI |
        '$recovery' LPAREN controlled_timing_check_event COMMA
            timing_check_event COMMA timing_check_limit
            ( COMMA notify_register )? RPAREN SEMI |
        '$setuphold' LPAREN timing_check_event COMMA timing_check_event COMMA
            timing_check_limit COMMA timing_check_limit
            ( COMMA notify_register )? RPAREN SEMI
        ;
sdpd :
	'if' LPAREN expression RPAREN
	simple_path_declaration
	SEMI
	;

level_sensitive_path_declaration :
	(parallel_level_sensitive_path_description) =>
	parallel_level_sensitive_path_description
	     ASSIGN path_delay_value SEMI
      |
	full_level_sensitive_path_description
	     ASSIGN path_delay_value SEMI
        ;

edge_sensitive_path_declaration :
	( 'if' LPAREN expression RPAREN )?
        LPAREN (edge_identifier)? specify_terminal_descriptor
	   ( PPATH | FPATH )
	   LPAREN ( (list_of_path_terminals) => list_of_path_terminals |
	            specify_terminal_descriptor )
	      (polarity_operator)? COLON data_source_expression
	   RPAREN
	RPAREN
	ASSIGN path_delay_value SEMI
        ;

timing_check_event :
        (timing_check_event_control)? specify_terminal_descriptor
        ( '&&&' timing_check_condition )?
        ;


parallel_level_sensitive_path_description :
        'if' LPAREN expression RPAREN
	   LPAREN specify_terminal_descriptor (polarity_operator)?
	          PPATH specify_terminal_descriptor RPAREN
	;

full_level_sensitive_path_description :
        'if' LPAREN expression RPAREN
	   LPAREN list_of_path_terminals (polarity_operator)?
	          FPATH list_of_path_terminals RPAREN
	;

polarity_operator :
	PLUS |
	MINUS
	;

edge_identifier :
        'posedge' |
        'negedge'
        ;

timing_check_event_control :
        'posedge' |
        'negedge' |
        edge_control_specifier
        ;
data_source_expression :
        expression
        ;
edge_control_specifier :
        'edge' LBRACK edge_descriptor ( COMMA edge_descriptor )* RBRACK
        ;

controlled_timing_check_event :
        timing_check_event_control specify_terminal_descriptor
        ( '&&&' timing_check_condition )?
        ;


timing_check_condition :
        scalar_timing_check_condition
        ; 
scalar_timing_check_condition :
        expression
        ;

timing_check_limit :
        expression
        ;

notify_register :
        name_of_register
    ;

   // Use semantic predicates to determine whether a matched
   // NUMBER or IDENTIFIER is a valid special value in the given context.
   // This kludge avoids having the special values in the Literals table,
   // thus avoiding a lexical conflict.
edge_descriptor :
	'0x' | '1x'
    |
        (n=NUMBER) { $n.getText()=="01" || $n.getText()=="10"}?
    |
        (i=IDENTIFIER) { $i.getText()=="x1" || $i.getText()=="x0"}?
        
	;









conditional_statement :
        'if' LPAREN expression RPAREN statement_or_null
//        ( options { warnWhenFollowAmbig = false; } :
	  'else' statement_or_null
//	)?
        ;

case_statement :
        case_keyword LPAREN expression RPAREN (case_item)+ 'endcase'
        ;

case_keyword :
        'case' | 'casez' | 'casex'
        ;

loop_statement :
        'forever' statement |
        'repeat' LPAREN expression RPAREN statement |
        'while' LPAREN expression RPAREN statement |
        'for' LPAREN assignment SEMI expression SEMI assignment RPAREN statement
        ;

procedural_timing_control_statement :
        delay_or_event_control statement_or_null
        ;

wait_statement :
        'wait' LPAREN expression RPAREN statement_or_null
        ;

event_trigger :
        TRIGGER name_of_event SEMI
        ;

disable_statement :
        'disable' IDENTIFIER SEMI
        ;

procedural_continuous_assignment :
        'assign' assignment SEMI |
        'deassign' lvalue SEMI |
        'force' assignment SEMI |
        'release' lvalue SEMI
        ;





range_or_type :
        range |
        'integer' |
        'real'
        ;




	// expression below isn't optional according to Palnitkar, but
	// several examples generated by Cadence use this syntax.
named_port_connection :
        DOT IDENTIFIER LPAREN (expression)? RPAREN
        ;







//----------------------------------------------------------------------------
// 7. Expressions
//----------------------------------------------------------------------------

lvalue :
	(identifier range) =>
        identifier range |
        identifier |
        concatenation
        ;

concatenation :
	(LCURLY expression LCURLY) =>
        LCURLY expression
	       LCURLY expression ( COMMA expression )* RCURLY RCURLY |
        LCURLY expression ( COMMA expression )* RCURLY
        ;

mintypmax_expression :
        expression ( COLON expression COLON expression )?
        ;
identifier :
        identifier_path
        ;

identifier_path :
        local_identifier ( DOT  local_identifier )*
        ;

expression_list :
        expression ( COMMA expression )*
        ;

function_call :
        name_of_function LPAREN expression_list RPAREN |
        name_of_system_task ( LPAREN expression_list RPAREN )?
        ;


exp11 :
        STRING |
	NUMBER |
	(function_call) => function_call |
	lvalue |
	DEFINE
        ;

exp10 :
        exp11 | LPAREN expression RPAREN
        ;

exp9 :
        exp10 | unary_operator exp9
        ;

exp8 :
        exp9 ( binary_operator exp9 )*
        ;

exp7 :
        exp8 ( QUESTION exp7 COLON exp7 )?
        ;

exp0 :
        exp7
        ;

expression :
        exp0
        ;


    // string literals
STRING :
        '"' (~('"'|'\n'))* '"'
        ;


   // a numeric literal
NUMBER :
	( (SIZE)? BASE SIZED_DIGIT ) => SIZED_NUMBER |
	UNSIZED_NUMBER
    ;


//
// NUMBER fragments
//
fragment
BASE :
	'\'' ( 'd' | 'D' | 'h' | 'H' | 'o' | 'O' | 'b' | 'B' )
	;

fragment
SIZED_DIGIT :
	DIGIT | HEXDIGIT | 'x' | 'X' | 'z' | 'Z' | '?'
	;

fragment
UNSIZED_NUMBER :
	DIGIT (DIGIT | '_')* ( '.' (DIGIT | '_')* )? (EXPONENT)?
        ;

fragment
DIGIT :
        ('0'..'9')
        ;

fragment
HEXDIGIT :
        ('A'..'F'|'a'..'f')
        ;

fragment
EXPONENT :
        ('e'|'E') ('+'|'-')? ('0'..'9')+
        ;

fragment
SIZE :
	(DIGIT)+
	;


fragment
SIZED_NUMBER :
	(SIZE)? BASE SIZED_DIGIT (SIZED_DIGIT | '_')*
	;



unary_operator :
        PLUS   |
        MINUS  |
        LNOT   |
        BNOT   |
        BAND   |
        RNAND  |
        BOR    |
        RNOR   |
        BXOR   |
        XNOR 
        ;

binary_operator :
        PLUS        |
        MINUS       |
        STAR        |
        DIV         |
        MOD         |
        EQUAL       |
        NOT_EQ      |
        EQ_CASE     |
        NOT_EQ_CASE |
        LAND        |
        LOR         |
        LT_         |
        LE          |
        GT          |
        GE          |
        BAND        |
        BOR         |
        BXOR        |
        XNOR       |
        SR          |
        SL 
        ;




//----------------------------------------------------------------------------
// 8. General
//----------------------------------------------------------------------------

    // Single-line comments
SL_COMMENT :
        '//' (~'\n')* '\n'
        { skip(); }
        ;

    // multiple-line comments
ML_COMMENT
    :    '/*'
        (    { input.LA(2)!='/' }? '*'
        |    '\n'  
        |    ~('*'|'\n')
        )*
        '*/'
            { skip(); }
    ;

delay_control :
        (POUND NUMBER) =>  POUND NUMBER
    |   (POUND identifier) => POUND identifier
    |   POUND LPAREN mintypmax_expression RPAREN
         ;

event_control :
        (AT identifier) => AT identifier
        | AT LPAREN event_expression RPAREN
        ;

event_expression :
        sub_event_expression ( 'or' sub_event_expression )*
        ;

sub_event_expression :
        expression |
        'posedge' expression |
        'negedge' expression
        ;



























    // "compiler" define/macro.
DEFINE
      
    :
	'`' IDENTIFIER
	;



//----------------------------------------------------------------------------
// Identifiers
//----------------------------------------------------------------------------

real_identifier :           identifier ;
//net_identifier :            identifier ;
//specparam_identifier :      identifier ;
udp_name_of_port :          identifier ;




//----------------------------------------------------------------------------
// Compiler directives
//----------------------------------------------------------------------------

directive:
	define_directive |
	include_directive
	;

define_directive :
	'`define' IDENTIFIER expression
	;

include_directive :
	'`include' ( identifier | STRING )
	;



udp_port_list :
        udp_name_of_port ( COMMA udp_name_of_port )*
        ;







WS  :   (' '|'\t' |'\n' | '\r' )+ {skip();};


IDENTIFIER    : 
        ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'$'|'0'..'9')*  
        ;

ESCAPED_IDENTIFIER :
        '\\'! (~ ' ')+ (' '|'\t'|'\n')!
        ;

  // Operators

LPAREN	    : '('  ;
RPAREN	    : ')'  ;
SEMI	    : ';'  ;
COMMA	    : ','  ;
DOT	   : '.'   ;
AT	    : '@'   ;
COLON	    : ':'   ;
ASSIGN	    : '='   ;
MINUS	    : '-'   ;
LBRACK	    : '['   ;
RBRACK	    : ']'   ;
LCURLY	    : '{'   ;
RCURLY	    : '}'   ;
POUND	    : '#'   ;
QUESTION    : '?'   ;
PLUS        : '+'   ;
LNOT        : '!'   ;
BNOT        : '~'   ;
BAND        : '&'   ;
RNAND       : '~&'  ;
BOR         : '|'   ;
RNOR        : '~|'  ;
BXOR        : '^'   ;
XNOR       : '~^' | '^~' ;
STAR        : '*'   ;
DIV         : '/'   ;
MOD         : '%'   ;
EQUAL       : '=='  ;
NOT_EQ      : '!='  ;
NOT_EQ_CASE : '!==' ;
EQ_CASE     : '===' ;
LAND        : '&&'  ;
LOR         : '||'  ;
LT_         : '<'   ;
LE          : '<='  ;
GT          : '>'   ;
GE          : '>='  ;
SR          : '>>'  ;
SL          : '<<'  ;
TRIGGER     : '->'  ;
PPATH       : '=>'  ;
FPATH       : '*>'  ;

list_of_assigned_variables :
        name_of_variable ( ASSIGN expression )?
	( COMMA name_of_variable ( ASSIGN expression )? )*
        ;



/*
// General - Attributes, Comments, Identifiers, Whitesapce

// Attributes

attribute_instance : '(*' attr_spec ( COMMA attr_spec )* '*)'
;
attr_spec : attr_name ( '=' constant_expression )?
;
attr_name : identifier
;

// Identifiers

array_identifier : identifier
    ;
block_identifier : identifier
    ;
bin_identifier : identifier
    ;
cell_identifier : identifier
    ;
class_identifier : identifier
    ;
class_variable_identifier : variable_identifier
    ;
clocking_identifier : identifier
    ;
config_identifier : identifier
    ;
constraint_identifier : identifier
    ;
covergroup_identifier : identifier
    ;
covergroup_variable_identifier : variable_identifier
    ;
cover_point_identifier : identifier
    ;
dynamic_array_variable_identifier : variable_identifier
    ;
enum_identifier : identifier
    ;
formal_identifier : identifier
    ;
function_identifier : identifier
    ;
generate_block_identifier : identifier
    ;
genvar_identifier : identifier
    ;

hierarchical_block_identifier : hierarchical_identifier
    ;
hierarchical_dynamic_array_variable_identifier : hierarchical_variable_identifier
    ;
hierarchical_event_identifier : hierarchical_identifier 
    ;
*/
local_identifier :
        IDENTIFIER 
    | ESCAPED_IDENTIFIER
    ;


hierarchical_identifier 
scope { 
boolean isTypedef;
}
@init  {
$declaration::isTypedef = false;
}
: 
        {$declaration::isTypedef=true;}
'$root'  ( '.' local_identifier ) +
;


hierarchical_net_identifier : 
(hierarchical_identifier) => hierarchical_identifier
 
    ;

hierarchical_parameter_identifier : hierarchical_identifier
    ;
hierarchical_task_identifier : hierarchical_identifier
    ;
hierarchical_tf_identifier : hierarchical_identifier
    ;
hierarchical_variable_identifier : hierarchical_identifier
    ;

index_variable_identifier : identifier
    ;

interface_identifier : identifier
    ;

interface_instance_identifier : identifier
    ;

inout_port_identifier : identifier
    ;

input_port_identifier : identifier
    ;

instance_identifier : identifier
    ;

library_identifier : identifier
    ;

member_identifier : identifier
    ;

method_identifier : identifier
    ;

modport_identifier : identifier
    ;

module_identifier : identifier
    ;

net_identifier : identifier
    ;

output_port_identifier : identifier
    ;

package_identifier : identifier
    ;

package_scope :
        (package_identifier '::') => package_identifier '::' 
    | '$unit ::'
    ;

parameter_identifier : identifier ;

port_identifier : identifier ;

production_identifier : identifier ;

program_identifier : identifier ;

property_identifier : identifier ;

//ps_class_identifier : ( package_scope )? class_identifier ;

//ps_covergroup_identifier : ( package_scope )? covergroup_identifier ;

//ps_identifier : ( package_scope )? identifier ;

//ps_or_hierarchical_net_identifier : ( package_scope )? net_identifier | hierarchical_net_identifier ;

//ps_or_hierarchical_tf_identifier : ( package_scope )? tf_identifier | hierarchical_tf_identifier ;

//ps_parameter_identifier : ( package_scope )? parameter_identifier ;

//ps_property_identifier : ( package_scope )? property_identifier ;

//ps_sequence_identifier : ( package_scope )? sequence_identifier ;

//ps_specparam_identifier : ( package_scope )? specparam_identifier ;

//ps_type_identifier : ( package_scope )? type_identifier ;


sequence_identifier : identifier ;

signal_identifier : identifier ;

specparam_identifier : identifier ;

system_tf_identifier : SYSTEM_TF_IDENTIFIER ;

SYSTEM_TF_IDENTIFIER    : 
        '$' ('a'..'z'|'A'..'Z'|'_'|'$') ('a'..'z'|'A'..'Z'|'_'|'$'|'0'..'9')*  
        ;

task_identifier : identifier ;

tf_identifier : identifier ;

terminal_identifier : identifier ;

topmodule_identifier : identifier ;

type_identifier : identifier ;

udp_identifier : identifier ;

variable_identifier : identifier ;


