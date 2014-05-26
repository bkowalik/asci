%{
#include <cstdio>
#include "plo.tab.h"

extern "C" int yylex();
extern "C" int yyparse();
extern "C" FILE *yyin;
%}


%union {
	int ival;
	float fval;
	char *sval;
}

%token MODULE
%token IDENT
%token BEGIN
%token END
%token CONST
%token SEMICOLON
%token COLON
%token INTEGER
%token VAR
%token PROCEDURE
%token LPARENTH
%token RPARENTH
%token OUTPUT
%token ASSIGN
%token IF THEN
%token WHILE DO
%token ODD
%token LEQ NEQ LSS GEQ GTR EQ
%token COMMA
%token PLUS MINUS TIMES SLASH
%token INPUT
%token DOT

%token <ival> INT
%token <fval> FLOAT
%token <sval> STRING

%%

Program : MODULE IDENT SEMICOLON Block IDENT DOT {};
Block : DeclList BEGIN StmtList END {};
DeclList : Decl SEMICOLON | DeclList Decl SEMICOLON {};
Decl : ConstDecl | ProcDecl | VarDecl {};
ConstDecl : CONST ConstDeclItem { , ConstDeclItem } {};
ConstDeclItem : IDENT COLON Type ASSIGN ConstExpr {};
ConstExpr : IDENT | INTEGER {};
VarDecl : VAR VarDeclItem { , VarDeclItem } {};
VarDeclItem : IDENT COLON Type {};
ProcDecl : PROCEDURE IDENT LPARENTH [ FormalDecl { , FormalDecl }] RPARENTH SEMICOLON Block IDENT {};
FormalDecl : IDENT COLON Type {};
Type : int {};
StmtList : { Stmt SEMICOLON } {};
Stmt : CallStmt | AssignStmt | OutStmt | IfStmt | WhileStmt {};
CallStmt : IDENT LPARENTH [ Exprs ] RPARENTH {};
AssignStmt : LValue ASSIGN Expr {};
LValue : IDENT {};
OutStmt : OUTPUT ASSIGN Expr {};
IfStmt : IF Test THEN StmtList END {};
WhileStmt : WHILE Test DO StmtList END {};
Test : ODD Sum | Sum Relop Sum {};
Relop : LEQ | NEQ | LSS | GEQ | GTR | EQ {};
Exprs : Expr { COMMA Expr } {};
Expr : Sum {};
Sum : Term { (PLUS | MINUS) Term } {};
Term : Factor { (TIMES | SLASH) Factor } {};
Factor : "-" Factor | LValue | INTEGER | INPUT | LPARENTH Expr RPARENTH {};

%%
int main(void) {
    yyparse();
}
