%code top{
    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    extern Ast ast;
    int yylex();
    int yyerror( char const * );
    int isVoidFunc;
    Type* returnType;
    int isReturn;
    std::vector<Type*> funcParamsType;
    std::vector<int> paramsNum;
    void searchVar(StmtNode*, Type*, SymbolTable*);
    void searchConst(StmtNode*, Type*, SymbolTable*);
}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"
}

%union {
    int itype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    Type* type;
}

%start Program
%token <strtype> ID 
%token <itype> INTEGER
%token IF ELSE WHILE BREAK CONTINUE
%token INT VOID
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA LBRACK RBRACK
%token ADD SUB OR AND LESS ASSIGN EQ GREATER NEQ MUL DIV GEQ LEQ MOD NOT
%token RETURN 
%token CONST
/*
%left ADD SUB
%left MUL DIV MOD
%right UMINUS UADD NOT*/

%nterm <stmttype> Stmts Stmt AssignStmt BlockStmt IfStmt ReturnStmt DeclStmt FuncDef VarDef VarDefs VarInit WhileStmt ConstDefs ConstDef ConstDecl FuncFParams FuncFParam  BreakStmt ContinueStmt ExpStmt EmptyStmt EmptyBlock
%nterm <exprtype> Exp AddExp Cond LOrExp PrimaryExp LVal RelExp LAndExp MulExp UnaryExp EqExp ConstInitVal LeafFunc FuncRParams ArrayStruct ArrayInit ArrayInits EmptyArrayStruct
%nterm <type> Type

%precedence THEN
%precedence ELSE
%%
Program
    : Stmts {
        ast.setRoot($1);
    }
    ;
Stmts
    : Stmt {$$=$1;}
    | Stmts Stmt{
        $$ = new SeqNode($1, $2);
    }
    ;
Stmt
    : AssignStmt {$$=$1;}
    | BlockStmt {$$=$1;}
    | IfStmt {$$=$1;}
    | ReturnStmt {$$=$1;}
    | DeclStmt {$$=$1;}
    | ConstDecl {$$=$1;};
    | FuncDef {$$=$1;}
    | WhileStmt {$$=$1;}
    | BreakStmt {$$=$1;}
    | ContinueStmt {$$=$1;};
    | ExpStmt {$$=$1;};
    | EmptyStmt {$$=$1;};
    | EmptyBlock {$$=$1;}
    ;
EmptyStmt
    : SEMICOLON{
        $$=new EmptyStmt();
    }
EmptyBlock
    : LBRACE RBRACE{
        $$=new EmptyBlock();
    }

ExpStmt
    : Exp SEMICOLON{
        
	isVoidFunc=0;
        $$=new ExpStmt($1);
    }

LVal
    : ID {
        SymbolEntry *se;
        se = identifiers->lookup($1);
        if(se == nullptr)
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }
        $$ = new Id(se);
        delete []$1;
    }
    |
    ID ArrayStruct{
        SymbolEntry *se;
        se = identifiers->lookup($1);
        if(se == nullptr)
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }
        if(!(dynamic_cast<IdentifierSymbolEntry*>(se)->getIsArray()))
        {
            fprintf(stderr, "identifier \"%s\" is not an array\n", (char*)$1);
            delete [](char*)$1;
            assert(dynamic_cast<IdentifierSymbolEntry*>(se)->getIsArray());
        }
        Id* it =new Id(se);
        it->setArrayStruct($2);
        it->setIsLval(1);
        $$ = it;
    }
    ;
AssignStmt
    :
    LVal ASSIGN Exp SEMICOLON {
        
        $$ = new AssignStmt($1, $3);
        if( isVoidFunc==1 )
        {
            fprintf(stderr, "function's type is void\n");
            assert(isVoidFunc!=1);
        }
	isVoidFunc=0;
    }
    ;
BlockStmt
    :   LBRACE 
        {identifiers = new SymbolTable(identifiers);} 
        Stmts RBRACE 
        {
            $$ = new CompoundStmt($3);
            SymbolTable *top = identifiers;
            identifiers = identifiers->getPrev();
            delete top;
        }
    ;
IfStmt
    : IF LPAREN Cond RPAREN Stmt %prec THEN {
        $$ = new IfStmt($3, $5);
    }
    | IF LPAREN Cond RPAREN Stmt ELSE Stmt {
        $$ = new IfElseStmt($3, $5, $7);
    }
    ;

WhileStmt
    : WHILE LPAREN Cond RPAREN Stmt {
        $$ = new WhileStmt($3, $5);
    }
    ;

BreakStmt
    : BREAK SEMICOLON {
        $$ = new BreakStmt();
    }
    ;

ContinueStmt
    : CONTINUE SEMICOLON {
        $$ = new ContinueStmt();
    }
    ;

ReturnStmt
    :
    RETURN Exp SEMICOLON{
        if(!returnType->isInt())
        {
            fprintf(stderr, "return type should be void!\n");
            assert(returnType->isInt());
        }
        
        
        isReturn=1;
        
        $$ = new ReturnStmt($2);

	if( isVoidFunc==1 )
        {
            fprintf(stderr, "function's type is void\n");
            assert(isVoidFunc!=1);
        }
	isVoidFunc=0;
    }
    | RETURN SEMICOLON {
        $$ = new ReturnStmt();
        if(!returnType->isVoid())
        {
            fprintf(stderr, "return type should be int!\n");
            assert(returnType->isVoid());
        }
        isReturn=1;
    }
    ;
Exp
    :
    AddExp {$$ = $1;}
    ;
Cond
    :
    LOrExp {$$ = $1;}
    ;
PrimaryExp
    :
    LVal {
        $$ = $1;
    }
    | INTEGER {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se);
    }
    |
    LPAREN Exp RPAREN{
        $$ =$2;
    }
    ;
AddExp
    :
    MulExp {$$ = $1;}
    |
    AddExp ADD MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
    }
    |
    AddExp SUB MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
    }
    ;
MulExp
    :
    UnaryExp {$$ = $1;}
    |
    MulExp MUL UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
    }
    |
    MulExp DIV UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
    }
    |
    MulExp MOD UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;
UnaryExp
    :
    PrimaryExp {$$ = $1;}
    |
    LeafFunc {$$ = $1;}
    |
    ADD UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::ADD, $2);
    }
    |
    SUB UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
    }
    |
    NOT UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::NOT, $2);
    }
    ;
RelExp
    :
    AddExp {$$ = $1;}
    |
    RelExp LESS AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
    }
    |
    RelExp GREATER AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3);
    }
    |
    RelExp LEQ AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LEQ, $1, $3);
    }
    |
    RelExp GEQ AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GEQ, $1, $3);
    }
    ;
EqExp
    :
    RelExp {$$ = $1;}
    |
    EqExp EQ RelExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::EQ, $1, $3);
    }
    |
    EqExp NEQ RelExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::NEQ, $1, $3);
    }
    ;
LAndExp
    :
    EqExp {$$ = $1;}
    |
    LAndExp AND EqExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
    }
    ;
LOrExp
    :
    LAndExp {$$ = $1;}
    |
    LOrExp OR LAndExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
    }
    ;
Type
    : INT {
        $$ = TypeSystem::intType;
    }
    | VOID {
        $$ = TypeSystem::voidType;
    }
    ;
DeclStmt
    :
    Type VarDefs SEMICOLON {
        StmtNode* it=$2;
        searchVar(it, $1, identifiers);
        $$ = new DeclStmt($2);
    }
    ;

VarDefs
    :
    VarDef {
        $$=$1;
    }
    |
    VarInit { 
        $$=$1;
    }
    |
    VarDefs COMMA VarDef {
        $$=new VarDefs($1, $3);
    }
    |
    VarDefs COMMA VarInit {
        $$=new VarDefs($1, $3);
    } 
    ;
VarDef
    :
    ID {
        $$=new VarDef($1);

    }
    |
    ID ArrayStruct{
        $$=new VarDef($1);
        $$->markArray(1);
        $$->setConstNum($2);
    }
    ;
ArrayStruct
    :
    LBRACK ConstInitVal RBRACK{
        $$=$2;
    }
    |
    ArrayStruct LBRACK ConstInitVal RBRACK{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$=new ArrayStruct(se,$1,$3);
    }
    ;
EmptyArrayStruct
    :
    LBRACK RBRACK{
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, -1);
        $$ = new Constant(se);
    }
    |
    EmptyArrayStruct LBRACK ConstInitVal RBRACK{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$=new ArrayStruct(se,$1,$3);
    }
    ;
VarInit
    :
    ID ASSIGN ArrayInit{
        $$=new VarInit($1, $3);

    }
    |
    ID ArrayStruct ASSIGN ArrayInit{
        $$=new VarInit($1, $4);
        $$->markArray(1);
        $$->setConstNum($2);
    }
    ;
ArrayInit
    :
    LBRACE RBRACE{
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        $$=new ArrayInit(se, new Constant(se),1);
    }
    |
    LBRACE ArrayInits RBRACE{
        $$=$2;
    }
    |
    Exp{
        SymbolEntry *se = dynamic_cast<ExprNode*>$1->getSymPtr();
        $$=new ArrayInit(se,$1,0);
    }
    ;
ArrayInits
    :
    ArrayInit{
        $$=$1;
    }
    |
    ArrayInits COMMA ArrayInit{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$=new ArrayInits(se,$1,$3);
    }
    ;

ConstDecl

    :

    CONST Type ConstDefs SEMICOLON {

        StmtNode* it=$3;

        searchConst(it, $2, identifiers);

        $$ = new ConstDecl($3);

    }

    ;



ConstDefs

    :

    ConstDef {

        $$=$1;

    }

    |

    ConstDefs COMMA ConstDef{

        $$=new ConstDefs($1, $3);

    }

    ;

ConstDef

    :

    ID ASSIGN ArrayInit{

        $$=new ConstDef($1, $3);

    }
    |
    ID ArrayStruct ASSIGN ArrayInit {
        $$=new ConstDef($1, $4);
        $$->markArray(1);
        $$->setConstNum($2);
    }
    ;

ConstInitVal

    :

    AddExp{

        $$=$1;

    }

    ;


FuncDef
    :
    Type ID LPAREN RPAREN{
        Type *funcType;

        funcParamsType.clear();
        
        funcType = new FunctionType($1,{});

        returnType=funcType->getRetType();
        isReturn=0;
        
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getLevel(), 0, 0);
        identifiers->install($2, se);
        identifiers = new SymbolTable(identifiers);
    }
    
    BlockStmt
    {
        SymbolEntry *se;
        se = identifiers->lookup($2);
        assert(se != nullptr);
        $$ = new FunctionDef(se, $6);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;

        if(0==isReturn){
          fprintf(stderr, "missing return!\n");
          assert(1==isReturn);
        }
    }
    | Type ID LPAREN {
        Type *funcType;

        funcParamsType.clear();

        funcType = new FunctionType($1,{});
        
        returnType=funcType->getRetType();
        isReturn=0;
         
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getLevel(), 0, 0);
        identifiers->install($2, se);
        identifiers = new SymbolTable(identifiers);
    }
    FuncFParams {
        SymbolEntry *se;
        se = identifiers->lookup($2);
        assert(se != nullptr);
        se->getType()->setParamsType(funcParamsType);

    }
    RPAREN
    BlockStmt
    {
        SymbolEntry *se;
        se = identifiers->lookup($2);
        assert(se != nullptr);
        $$ = new FunctionDef(se, $5 , $8);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        //delete []$2;
        
        if(0==isReturn){
          fprintf(stderr, "missing return!\n");
          assert(1==isReturn);
        }
    }
    ;
FuncFParams
    : FuncFParam {$$=$1;}
    | FuncFParams COMMA FuncFParam{
        $$ = new FuncFParams($1, $3);
    }
    ;
FuncFParam
    : Type ID {
        funcParamsType.push_back($1);
        SymbolEntry *se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel(), 0, 0);
        identifiers->install($2, se);
        $$=new FuncFParam($2, se, 0);
    }
    | Type ID EmptyArrayStruct{
        funcParamsType.push_back($1);
        SymbolEntry *se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel(), 0, 1);
        identifiers->install($2, se);
        $$=new FuncFParam($2, se, 1);
        $$->markArray(1);
        
    }
    ;


LeafFunc
    : ID LPAREN RPAREN {
        Type *funcType;
        
        funcType = new FunctionType(NULL,{});
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $1, identifiers->getLevel(), 0, 0);

        SymbolEntry *se1;
        se1 = identifiers->lookup($1);
        if(se1 == nullptr || !se1->getType()->isFunc())
        {
            fprintf(stderr, "function \"%s\" is undefined\n", (char*)$1);
            delete [](char*)$1;
            assert(se1 != nullptr && se->getType()->isFunc());
        }

        
        if(se1->getType()->getRetType()->isVoid()){isVoidFunc=1;}
        else{isVoidFunc=0;}

        $$ = new LeafFunc(se1,$1);
        
    }
    | ID LPAREN { paramsNum.push_back(0); }
      FuncRParams RPAREN {
        Type *funcType;
        
        int num=paramsNum[paramsNum.size()-1];
        funcType = new FunctionType(NULL,std::vector<Type*>(funcParamsType.end()-num, funcParamsType.end()));
        for(int i=0;i<num;i++)
        {
            funcParamsType.pop_back();
        }
        paramsNum.pop_back();
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $1, identifiers->getLevel(), 0, 0);

        SymbolEntry *se1;
        se1 = identifiers->lookup($1);
        if(se1 == nullptr || !se1->getType()->isFunc() )
        {
            fprintf(stderr, "function \"%s\" is undefined\n", (char*)$1);
            delete [](char*)$1;
            assert(se1 != nullptr && se1->getType()->isFunc());
        }
        else if(se->getType()->paramsLen()!=se1->getType()->paramsLen())
        {
            fprintf(stderr, "function \"%s\" params do not match\n", (char*)$1);
            delete [](char*)$1;
            assert(se->getType()->paramsLen()==se1->getType()->paramsLen());
        }

        

        if(se1->getType()->getRetType()->isVoid()){isVoidFunc=1;}
        else{isVoidFunc=0;}

        $$ = new LeafFunc(se1,$1 ,$4);
        
        

    }
    ;

FuncRParams
    : Exp {
        $$=$1; 
        funcParamsType.push_back(TypeSystem::intType);
        paramsNum[paramsNum.size()-1]++;
    }
    | FuncRParams COMMA Exp{
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new FuncRParams(se,$1, $3);
        funcParamsType.push_back(TypeSystem::intType);
        paramsNum[paramsNum.size()-1]++;
    }
    ;

%%
void init()
{
Type *funcType;
char* a=(char*)"putint";
char* b=(char*)"getint";
char* c=(char*)"putch";
char* d=(char*)"getch";
funcParamsType.clear();
funcParamsType.push_back(TypeSystem::intType);
funcType = new FunctionType(TypeSystem::voidType,{});
SymbolEntry *se = new IdentifierSymbolEntry(funcType, a, identifiers->getLevel(), 0, 0);
se->getType()->setParamsType(funcParamsType);
identifiers->install(a, se);
funcParamsType.clear();
funcParamsType.push_back(TypeSystem::voidType);
funcType = new FunctionType(TypeSystem::intType,{});
SymbolEntry *se1 = new IdentifierSymbolEntry(funcType, b, identifiers->getLevel(), 0, 0);
se1->getType()->setParamsType(funcParamsType);
identifiers->install(b, se1);
funcParamsType.clear();
funcParamsType.push_back(TypeSystem::intType);
funcType = new FunctionType(TypeSystem::voidType,{});
SymbolEntry *se2 = new IdentifierSymbolEntry(funcType, c, identifiers->getLevel(), 0, 0);
se2->getType()->setParamsType(funcParamsType);
identifiers->install(c, se2);
funcParamsType.clear();
funcParamsType.push_back(TypeSystem::voidType);
funcType = new FunctionType(TypeSystem::intType,{});
SymbolEntry *se3 = new IdentifierSymbolEntry(funcType, d, identifiers->getLevel(), 0, 0);
se3->getType()->setParamsType(funcParamsType);
identifiers->install(d, se3);
}


int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}

void searchVar(StmtNode* it, Type* type, SymbolTable* identifiers)
{
    if(it->isLeaf()==1)
    {
        SymbolEntry *se;
        SymbolEntry* se1=identifiers->lookup(it->getId());
        if(se1!=nullptr && dynamic_cast<IdentifierSymbolEntry*>(se1)->getScope()==identifiers->getLevel())
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)it->getId());
            assert(dynamic_cast<IdentifierSymbolEntry*>(se1)->getScope()!=identifiers->getLevel());
        }
        se = new IdentifierSymbolEntry(type, it->getId(), identifiers->getLevel(), 0, it->getIsArray());
        identifiers->install(it->getId(), se); 
        it->setId(new Id(se));  
    }
    else
    {
        if(it->isLeaf()==2)
        {
            SymbolEntry *se;
            SymbolEntry* se1=identifiers->lookup(it->getId());
            if(se1!=nullptr && dynamic_cast<IdentifierSymbolEntry*>(se1)->getScope()==identifiers->getLevel())
            {
                fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)it->getId());
                assert(dynamic_cast<IdentifierSymbolEntry*>(se1)->getScope()!=identifiers->getLevel());
            }
            se = new IdentifierSymbolEntry(type, it->getId(), identifiers->getLevel(), 0, it->getIsArray());
            identifiers->install(it->getId(), se); 
            it->setId(new Id(se));
        }
        else
        {
            searchVar(it->getV1(), type, identifiers);
            searchVar(it->getV2(), type, identifiers);
        }
    }

}

void searchConst(StmtNode* it, Type* type, SymbolTable* identifiers)
{
    if(it->isLeaf()==1)
    {
        /*
        SymbolEntry *se;
        se = identifiers->lookup(it->id);
        dynamic_cast<IdentifierSymbolEntry*>(se)->markConst(1);*/
        SymbolEntry *se;
        SymbolEntry* se1=identifiers->lookup(it->getId());
        if(se1!=nullptr && dynamic_cast<IdentifierSymbolEntry*>(se1)->getScope()==identifiers->getLevel())
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)it->getId());
            assert(dynamic_cast<IdentifierSymbolEntry*>(se1)->getScope()!=identifiers->getLevel());
        }
        se = new IdentifierSymbolEntry(type, it->getId(), identifiers->getLevel(), 1, it->getIsArray());
        identifiers->install(it->getId(), se); 
        it->setId(new Id(se));  
    }
    else
    {
        searchConst(it->getC1(), type, identifiers);
        searchConst(it->getC2(), type, identifiers);
    }

}

