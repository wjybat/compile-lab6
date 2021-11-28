#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include "Operand.h"

class SymbolEntry;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class IRBuilder;

class Node
{
private:
    static int counter;
    int seq;
protected:
    std::vector<Instruction*> true_list;
    std::vector<Instruction*> false_list;
    static IRBuilder *builder;
    void backPatch(std::vector<Instruction*> &list, BasicBlock*bb);
    void backPatchF(std::vector<Instruction*> &list, BasicBlock*bb);
    std::vector<Instruction*> merge(std::vector<Instruction*> &list1, std::vector<Instruction*> &list2);

public:
    Node();
    int getSeq() const {return seq;};
    static void setIRBuilder(IRBuilder*ib) {builder = ib;};
    virtual void output(int level) = 0;
    virtual void typeCheck() {};
    virtual void genCode() {};
    std::vector<Instruction*>& trueList() {return true_list;}
    std::vector<Instruction*>& falseList() {return false_list;}
};

class ExprNode : public Node
{
protected:
    SymbolEntry *symbolEntry;
    Operand *dst;   // The result of the subtree is stored into dst.
public:
    ExprNode(SymbolEntry *symbolEntry) : symbolEntry(symbolEntry){};
    Operand* getOperand() {return dst;};
    SymbolEntry* getSymPtr() {return symbolEntry;};
};

class BinaryExpr : public ExprNode
{
private:
    int op;
    ExprNode *expr1, *expr2;
public:
    enum {ADD, SUB, AND, OR, EQ, LESS, GREATER, MUL, DIV, MOD, NEQ, LEQ, GEQ};
    BinaryExpr(SymbolEntry *se, int op, ExprNode*expr1, ExprNode*expr2) : ExprNode(se), op(op), expr1(expr1), expr2(expr2){dst = new Operand(se);};
    void output(int level);
    void typeCheck();
    void genCode();
};

class Constant : public ExprNode
{
public:
    Constant(SymbolEntry *se) : ExprNode(se){dst = new Operand(se);};
    void output(int level);
    void typeCheck();
    void genCode();
};

class UnaryExpr : public ExprNode
{
private:
    int op;
    ExprNode *expr;
public:
    enum {ADD, SUB, NOT};
    UnaryExpr(SymbolEntry *se, int op, ExprNode*expr) : ExprNode(se), op(op), expr(expr){dst = new Operand(se);};
    void output(int level);
};

class Id : public ExprNode
{
private:
    ExprNode *expr;
    bool isLval;
public:
    Id(SymbolEntry *se) : ExprNode(se), expr(nullptr), isLval(0){SymbolEntry *temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel()); dst = new Operand(temp);};
    void output(int level);
    void setArrayStruct(ExprNode* expr);
    void setIsLval(bool isLval);
    void typeCheck();
    void genCode();
};

class ArrayStruct : public ExprNode
{
private:
    ExprNode *expr1, *expr2;
public:
    ArrayStruct(SymbolEntry *se, ExprNode* expr1, ExprNode* expr2) : ExprNode(se), expr1(expr1), expr2(expr2){dst = new Operand(se);};
    void output(int level);
};
class ArrayVal : public ExprNode
{
private:
    ExprNode *expr1, *expr2;
public:
    ArrayVal(SymbolEntry *se, ExprNode* expr1, ExprNode* expr2) : ExprNode(se), expr1(expr1), expr2(expr2){dst = new Operand(se);};
    void output(int level);
};

class ArrayInit : public ExprNode
{
private:
    ExprNode *expr;
    bool isArray;
public:
    ArrayInit(SymbolEntry *se, ExprNode* expr,bool isArray) : ExprNode(se), expr(expr), isArray(isArray){dst = new Operand(se);};
    void output(int level);
};

class ArrayInits : public ExprNode
{
private:
    ExprNode *expr1, *expr2;
public:
    ArrayInits(SymbolEntry *se, ExprNode* expr1, ExprNode* expr2) : ExprNode(se), expr1(expr1), expr2(expr2){dst = new Operand(se);};
    void output(int level);
};

class StmtNode : public Node
{
public:
    virtual int isLeaf();
    virtual char* getId();
    virtual StmtNode* getV1();
    virtual StmtNode* getV2();
    virtual StmtNode* getC1();
    virtual StmtNode* getC2();
    virtual void setId(Id * id);
    virtual void markArray(bool array);
    virtual void setConstNum(ExprNode* num);
    virtual bool getIsArray();

};

class CompoundStmt : public StmtNode
{
private:
    StmtNode *stmt;
public:
    CompoundStmt(StmtNode *stmt) : stmt(stmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class SeqNode : public StmtNode
{
private:
    StmtNode *stmt1, *stmt2;
public:
    SeqNode(StmtNode *stmt1, StmtNode *stmt2) : stmt1(stmt1), stmt2(stmt2){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class VarDefs : public StmtNode
{
private:
    StmtNode *VarDefs1, *VarDefs2;
    int leaf;
public:
    VarDefs(StmtNode* VarDefs1, StmtNode* VarDefs2) : VarDefs1(VarDefs1), VarDefs2(VarDefs2), leaf(0){};
    int isLeaf() {return leaf;}
    StmtNode* getV1() {return VarDefs1;};
    StmtNode* getV2() {return VarDefs2;};
    void output(int level);
    void genCode();
};

class VarDef : public StmtNode
{
private:
    char *id;
    int leaf;
    Id *rid;
    bool isArray;
    ExprNode* constNum;
public:
    VarDef(char *id) : id(id), leaf(1), rid(NULL), isArray(0), constNum(NULL){};
    char* getId() {return id;};
    int isLeaf() {return leaf;}
    void output(int level);
    void setId(Id * id){rid=id;};
    void markArray(bool array){isArray=array;};
    bool getIsArray(){return isArray;};
    void setConstNum(ExprNode* num){constNum=num;};
    void genCode();
};

class VarInit : public StmtNode
{
private:
    char *id;
    ExprNode *expr;
    int leaf;
    Id *rid;
    bool isArray;
    ExprNode* constNum;
public:
    VarInit(char *id, ExprNode *expr) : id(id), expr(expr), leaf(2), rid(NULL), isArray(0), constNum(NULL){};
    char* getId() {return id;};
    int isLeaf() {return leaf;}
    void output(int level);
    void setId(Id * id){rid=id;};
    void markArray(bool array){isArray=array;};
    bool getIsArray(){return isArray;};
    void setConstNum(ExprNode* num){constNum=num;};
    void genCode();
};

class ConstDefs : public StmtNode
{
private:
    StmtNode *ConstDefs1, *ConstDefs2;
    int leaf;
public:
    ConstDefs(StmtNode* ConstDefs1, StmtNode* ConstDefs2) : ConstDefs1(ConstDefs1), ConstDefs2(ConstDefs2), leaf(0){};
    int isLeaf() {return leaf;}
    StmtNode* getC1() {return ConstDefs1;};
    StmtNode* getC2() {return ConstDefs2;};
    void output(int level);
};

class ConstDef : public StmtNode
{
private:
    char *id;
    int leaf;
    Id *rid;
    ExprNode* expr;
    bool isArray;
    ExprNode* constNum;
public:
    ConstDef(char *id, ExprNode* expr) : id(id), leaf(1), rid(NULL), expr(expr),isArray(0), constNum(NULL){};
    char* getId() {return id;};
    int isLeaf() {return leaf;}
    void output(int level);
    void setId(Id* id){rid=id;};
    void markArray(bool array){isArray=array;};
    bool getIsArray(){return isArray;};
    void setConstNum(ExprNode* num){constNum=num;};
};


class DeclStmt : public StmtNode
{
private:
    StmtNode *varDefs;
public:
    DeclStmt(StmtNode *varDefs) : varDefs(varDefs){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ConstDecl : public StmtNode
{
private:
    StmtNode *constDefs;
public:
    ConstDecl(StmtNode *constDefs) : constDefs(constDefs){};
    void output(int level);
};

class IfStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
public:
    IfStmt(ExprNode *cond, StmtNode *thenStmt) : cond(cond), thenStmt(thenStmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class IfElseStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
    StmtNode *elseStmt;
public:
    IfElseStmt(ExprNode *cond, StmtNode *thenStmt, StmtNode *elseStmt) : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class WhileStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
public:
    WhileStmt(ExprNode *cond, StmtNode *thenStmt) : cond(cond), thenStmt(thenStmt){};
    void output(int level);
};

class ReturnStmt : public StmtNode
{
private:
    ExprNode *retValue;
public:
    ReturnStmt(ExprNode*retValue) : retValue(retValue) {};
    ReturnStmt() : retValue(nullptr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class AssignStmt : public StmtNode
{
private:
    ExprNode *lval;
    ExprNode *expr;
public:
    AssignStmt(ExprNode *lval, ExprNode *expr) : lval(lval), expr(expr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ExpStmt : public StmtNode
{
private:
    ExprNode *expr;
public:
    ExpStmt(ExprNode *expr) : expr(expr) {};
    void output(int level);
};

class EmptyStmt : public StmtNode
{
public:
    EmptyStmt(){};
    void output(int level);
};

class EmptyBlock : public StmtNode
{
public:
    EmptyBlock(){};
    void output(int level);
};

class FunctionDef : public StmtNode
{
private:
    SymbolEntry *se;
    StmtNode *params;
    StmtNode *stmt;
public:
    FunctionDef(SymbolEntry *se, StmtNode *stmt) : se(se),params(nullptr),stmt(stmt){};
    FunctionDef(SymbolEntry *se, StmtNode *params, StmtNode *stmt) : se(se), params(params), stmt(stmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class BreakStmt : public StmtNode
{
private:
    
public:
    BreakStmt()  {};
    void output(int level);
};

class ContinueStmt : public StmtNode
{
private:
    
public:
    ContinueStmt() {};
    void output(int level);
};

class FuncFParams : public StmtNode
{
private:
    StmtNode *FuncFParams1, *FuncFParams2;
    int leaf;
public:
    FuncFParams(StmtNode* FuncFParams1, StmtNode* FuncFParams2) : FuncFParams1(FuncFParams1), FuncFParams2(FuncFParams2), leaf(0){};
    int isLeaf() {return leaf;}
    StmtNode* getFP1() {return FuncFParams1;};
    StmtNode* getFP2() {return FuncFParams2;};
    void output(int level);
};

class FuncFParam : public StmtNode
{
private:
    char *id;
    int leaf;
    SymbolEntry *se;
    bool isArray;
    ExprNode* constNum;
public:
    FuncFParam(char *id, SymbolEntry *se, bool array) : id(id), leaf(1), se(se), isArray(array), constNum(NULL){};
    FuncFParam(char *id, SymbolEntry *se, bool array, ExprNode* num) : id(id), leaf(1), se(se), isArray(array), constNum(num){};
    char* getId() {return id;};
    int isLeaf() {return leaf;}
    void output(int level);
    void markArray(bool array){isArray=array;};
    bool getIsArray(){return isArray;};
    void setConstNum(ExprNode* num){constNum=num;};
};

class LeafFunc : public ExprNode
{
private:
    char *id;
    ExprNode *params;
public:
    LeafFunc(SymbolEntry *se, char *id) : ExprNode(se),id(id), params(NULL){};
    LeafFunc(SymbolEntry *se, char *id, ExprNode *params) : ExprNode(se),id(id), params(params){};
    void output(int level);
};

class FuncRParams : public ExprNode
{
private:
    ExprNode *Exp1, *Exp2;
    int leaf;
public:
    FuncRParams(SymbolEntry *se, ExprNode *Exp1, ExprNode *Exp2) : ExprNode(se),Exp1(Exp1), Exp2(Exp2), leaf(0){};
    int isLeaf() {return leaf;}
    ExprNode* getE1() {return Exp1;};
    ExprNode* getE2() {return Exp2;};
    void output(int level);
};

class Ast
{
private:
    Node* root;
public:
    Ast() {root = nullptr;}
    void setRoot(Node*n) {root = n;}
    void output();
    void typeCheck();
    void genCode(Unit *unit);
};

#endif
