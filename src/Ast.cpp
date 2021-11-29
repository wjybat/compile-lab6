#include "Ast.h"
#include "SymbolTable.h"
#include "Unit.h"
#include "Instruction.h"
#include "IRBuilder.h"
#include <string>
#include "Type.h"
#include <assert.h>

extern FILE *yyout;
int Node::counter = 0;
IRBuilder* Node::builder = nullptr;

Node::Node()
{
    seq = counter++;
}

void Node::backPatch(std::vector<Instruction*> &list, BasicBlock*bb)
{
    for(auto &inst:list)
    {
        if(inst->isCond())
            dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
        else if(inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}

void Node::backPatchF(std::vector<Instruction*> &list, BasicBlock*bb)
{
    for(auto &inst:list)
    {
        if(inst->isCond())
            dynamic_cast<CondBrInstruction*>(inst)->setFalseBranch(bb);
        else if(inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}

std::vector<Instruction*> Node::merge(std::vector<Instruction*> &list1, std::vector<Instruction*> &list2)
{
    std::vector<Instruction*> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Ast::genCode(Unit *unit)
{
    IRBuilder *builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

void FunctionDef::genCode()
{
    Unit *unit = builder->getUnit();
    Function *func = new Function(unit, se, params);
    BasicBlock *entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.
    builder->setInsertBB(entry);
    if(params!=nullptr)
    {
        params->genCode();
    }

    stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors for each basic block.
     * Todo
    */
   
}

void BinaryExpr::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Function *func = bb->getParent();
    if (op == AND)
    {
        BasicBlock *trueBB = new BasicBlock(func);  // if the result of lhs is true, jump to the trueBB.
        if(expr1->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(expr1->getSymPtr()->getType())->itsSize()==1)
        {
            expr1->genCode();
        }
        else
        {
            expr1->genCode();
            Operand* src1= expr1->getOperand();
            int opcode;
            opcode = CmpInstruction::NE;
            SymbolEntry* se;
            Operand* src2;
            se = new ConstantSymbolEntry(TypeSystem::intType, 0);
            src2=new Operand(se);
            new CmpInstruction(opcode, dst, src1, src2, bb);
            Instruction* temp=new CondBrInstruction(nullptr,nullptr,dst,builder->getInsertBB());
            expr1->trueList().push_back(temp);
            expr1->falseList().push_back(temp);
        }
        backPatch(expr1->trueList(), trueBB);
        trueBB->addPred(builder->getInsertBB());
        builder->getInsertBB()->addSucc(trueBB);
        builder->setInsertBB(trueBB);  
        if(expr2->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(expr2->getSymPtr()->getType())->itsSize()==1)
            expr2->genCode();
        else
        {
            expr2->genCode();
            Operand* src1= expr2->getOperand();
            int opcode;
            opcode = CmpInstruction::NE;
            SymbolEntry* se;
            Operand* src2;
            se = new ConstantSymbolEntry(TypeSystem::intType, 0);
            src2=new Operand(se);
            new CmpInstruction(opcode, dst, src1, src2, builder->getInsertBB());
            Instruction* temp=new CondBrInstruction(nullptr,nullptr,dst,builder->getInsertBB());
            expr2->trueList().push_back(temp);
            expr2->falseList().push_back(temp);
        }
            true_list = expr2->trueList();
            false_list = merge(expr1->falseList(), expr2->falseList());
    }
    else if(op == OR)
    {
        // Todo
        BasicBlock *trueBB = new BasicBlock(func);  // if the result of lhs is true, jump to the trueBB.
        if(expr1->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(expr1->getSymPtr()->getType())->itsSize()==1)
        {
            expr1->genCode();
        }
        else
        {
            expr1->genCode();
            Operand* src1= expr1->getOperand();
            int opcode;
            opcode = CmpInstruction::NE;
            SymbolEntry* se;
            Operand* src2;
            se = new ConstantSymbolEntry(TypeSystem::intType, 0);
            src2=new Operand(se);
            new CmpInstruction(opcode, dst, src1, src2, bb);
            Instruction* temp=new CondBrInstruction(nullptr,nullptr,dst,builder->getInsertBB());
            expr1->trueList().push_back(temp);
            expr1->falseList().push_back(temp);
        }
        backPatchF(expr1->falseList(), trueBB);
        trueBB->addPred(builder->getInsertBB());
        builder->getInsertBB()->addSucc(trueBB);
        builder->setInsertBB(trueBB);               // set the insert point to the trueBB so that intructions generated by expr2 will be inserted into it.
        if(expr2->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(expr2->getSymPtr()->getType())->itsSize()==1)
            expr2->genCode();
        else
        {
            expr2->genCode();
            Operand* src1= expr2->getOperand();
            int opcode;
            opcode = CmpInstruction::NE;
            SymbolEntry* se;
            Operand* src2;
            se = new ConstantSymbolEntry(TypeSystem::intType, 0);
            src2=new Operand(se);
            new CmpInstruction(opcode, dst, src1, src2, builder->getInsertBB());
            Instruction* temp=new CondBrInstruction(nullptr,nullptr,dst,builder->getInsertBB());
            expr2->trueList().push_back(temp);
            expr2->falseList().push_back(temp);
        }
        false_list = expr2->falseList();
        true_list = merge(expr1->trueList(), expr2->trueList());
    }
    else if(op >= EQ && op <= GEQ)
    {
        // Todo
        if(expr1->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(expr1->getSymPtr()->getType())->itsSize()==1)
        {
            expr1->genCode();
            BasicBlock *BB1 = new BasicBlock(func);
            BB1->addPred(builder->getInsertBB());
            builder->getInsertBB()->addSucc(BB1);
            backPatch(expr1->trueList(),BB1);
            backPatchF(expr1->falseList(),BB1);
            builder->setInsertBB(BB1);  
        }
        else
        {
            expr1->genCode();
        }
        if(expr2->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(expr2->getSymPtr()->getType())->itsSize()==1)
        {
            expr2->genCode();
            BasicBlock *BB2 = new BasicBlock(func);
            BB2->addPred(builder->getInsertBB());
            builder->getInsertBB()->addSucc(BB2);
            backPatch(expr2->trueList(),BB2);
            backPatchF(expr2->falseList(),BB2);
            builder->setInsertBB(BB2);  
        }
        else
        {
            expr2->genCode();
        }
        Operand *src1 = expr1->getOperand();
        Operand *src2 = expr2->getOperand();
        int opcode;
        switch (op)
        {
        case LESS:
            opcode = CmpInstruction::L;
            break;
        case GREATER:
            opcode = CmpInstruction::G;
            break;
        case EQ:
            opcode = CmpInstruction::E;
            break;
        case NEQ:
            opcode = CmpInstruction::NE;
            break;
        case LEQ:
            opcode = CmpInstruction::LE;
            break;
        case GEQ:
            opcode = CmpInstruction::GE;
            break;

        }
        new CmpInstruction(opcode, dst, src1, src2, builder->getInsertBB());
        Instruction* temp=new CondBrInstruction(nullptr,nullptr,dst,builder->getInsertBB());
        true_list.push_back(temp);
        false_list.push_back(temp);
    }
    else if(op >= ADD && op <= MOD)
    {
        expr1->genCode();
        expr2->genCode();
        Operand *src1 = expr1->getOperand();
        Operand *src2 = expr2->getOperand();
        int opcode;
        switch (op)
        {
        case ADD:
            opcode = BinaryInstruction::ADD;
            break;
        case SUB:
            opcode = BinaryInstruction::SUB;
            break;
        case MUL:
            opcode = BinaryInstruction::MUL;
            break;
        case DIV:
            opcode = BinaryInstruction::DIV;
            break;
        case MOD:
            opcode = BinaryInstruction::MOD;
            break;
        }
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
}

void UnaryExpr::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    if(op>=ADD && op<=SUB)
    {
        if(expr->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(expr->getSymPtr()->getType())->itsSize()!=1)
        {
            expr->genCode();
            true_list=expr->falseList();
            false_list=expr->trueList();
            Operand* src1=expr->getOperand();
            int opcode;
            SymbolEntry* se;
            Operand* src2;
            switch (op)
            {
            case ADD:
                opcode = BinaryInstruction::MUL;
                se = new ConstantSymbolEntry(TypeSystem::intType, 1);
                src2=new Operand(se);
                break;
            case SUB:
                opcode = BinaryInstruction::MUL;
                se = new ConstantSymbolEntry(TypeSystem::intType, -1);
                src2=new Operand(se);
                break;
            }
            new BinaryInstruction(opcode, dst, src2, src1, bb);
        }
        else
        {
            expr->genCode();
            BasicBlock *bb1 = builder->getInsertBB();
            bb1->remove(bb1->rbegin());
            true_list=expr->falseList();
            false_list=expr->trueList();
            Operand* src1=expr->getOperand();
            int opcode;
            SymbolEntry* se;
            Operand* src2;
            switch (op)
            {
            case ADD:
                opcode = BinaryInstruction::MUL;
                se = new ConstantSymbolEntry(TypeSystem::intType, 1);
                src2=new Operand(se);
                break;
            case SUB:
                opcode = BinaryInstruction::MUL;
                se = new ConstantSymbolEntry(TypeSystem::intType, -1);
                src2=new Operand(se);
                break;
            }
            new BinaryInstruction(opcode, dst, src2, src1, builder->getInsertBB());
        }
    }
    else
    {
        if(expr->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(expr->getSymPtr()->getType())->itsSize()!=1)
        {
            expr->genCode();
            Operand* src1= expr->getOperand();
            int opcode;
            opcode = CmpInstruction::E;
            SymbolEntry* se;
            Operand* src2;
            se = new ConstantSymbolEntry(TypeSystem::intType, 0);
            src2=new Operand(se);
            new CmpInstruction(opcode, dst, src1, src2, bb);
            Instruction* temp=new CondBrInstruction(nullptr,nullptr,dst,builder->getInsertBB());
            true_list.push_back(temp);
            false_list.push_back(temp);
        }
        else
        {
            Function *func = bb->getParent();
            BasicBlock *trueBB = new BasicBlock(func); 
            trueBB->addPred(builder->getInsertBB());
            builder->getInsertBB()->addSucc(trueBB);
            expr->genCode();
            backPatch(expr->trueList(),trueBB);
            backPatchF(expr->falseList(),trueBB);
            Operand* src1=expr->getOperand();
            int opcode;
            opcode = BinaryInstruction::SUB;
            SymbolEntry* se;
            se = new ConstantSymbolEntry(TypeSystem::intType, 1);
            Operand* src2=new Operand(se);
            builder->setInsertBB(trueBB);
            new BinaryInstruction(opcode, dst, src2, src1, builder->getInsertBB());
            Instruction* temp=new CondBrInstruction(nullptr,nullptr,dst,builder->getInsertBB());
            true_list.push_back(temp);
            false_list.push_back(temp);
        }
        
    }
    
}

void Constant::genCode()
{
    // we don't need to generate code.
}

void Id::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    std::string s=dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->toStr();
    assert(addr!=nullptr);
    if(!dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->isParam())
        new LoadInstruction(dst, addr, bb);
    else
    {
        dst=new Operand(symbolEntry);
    }
    
}

void LeafFunc::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Type* type=dynamic_cast<FunctionType*>(dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getType())->getRetType();
    if(type->isVoid())
    {
        if(params!=nullptr)
        {
            params->genCode();
        }
        new LeafVoidIns(symbolEntry, params, bb);
    }
    else
    {
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);                                              
        dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->setAddr(addr); 
        setOperand(addr);
        if(params!=nullptr)
        {
            params->genCode();
        }
        new LeafRetIns(addr, symbolEntry, params, bb);
    }
}

void FuncRParams::genCode()
{
    Exp1->genCode();
    Exp2->genCode();
}

void ExpStmt::genCode()
{
    expr->genCode();
}

void IfStmt::genCode()
{
    Function *func;
    BasicBlock *then_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    if(cond->getSymPtr()->getType()->isInt() && dynamic_cast<IntType*>(cond->getSymPtr()->getType())->itsSize()==1)
    {
        cond->genCode();
    }
    else
    {
        cond->genCode();
        Operand* src1= cond->getOperand();
        int opcode;
        opcode = CmpInstruction::NE;
        SymbolEntry* se;
        Operand* src2;
        se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        src2=new Operand(se);
        SymbolEntry* s=new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        Operand* dst=new Operand(s);

        new CmpInstruction(opcode, dst, src1, src2, builder->getInsertBB());
        Instruction* temp=new CondBrInstruction(nullptr,nullptr,dst,builder->getInsertBB());
        cond->trueList().push_back(temp);
        cond->falseList().push_back(temp);
    }
    backPatch(cond->trueList(), then_bb);
    backPatchF(cond->falseList(), end_bb);
    then_bb->addPred(builder->getInsertBB());
    end_bb->addPred(builder->getInsertBB());
    builder->getInsertBB()->addSucc(then_bb);
    builder->getInsertBB()->addSucc(end_bb);
    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);
}

//new added
void IfElseStmt::genCode()
{
    // Todo
    Function *func;
    BasicBlock *then_bb, *else_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();
    
    backPatch(cond->trueList(), then_bb);
    backPatchF(cond->falseList(), else_bb);
    
    then_bb->addPred(builder->getInsertBB());
    else_bb->addPred(builder->getInsertBB());
    end_bb->addPred(else_bb);
    
    builder->getInsertBB()->addSucc(then_bb);
    builder->getInsertBB()->addSucc(else_bb);
    else_bb->addSucc(end_bb);
     
    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);
    
    builder->setInsertBB(else_bb);
    elseStmt->genCode();
    else_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, else_bb);
    

    builder->setInsertBB(end_bb);
}
//new added
void WhileStmt::genCode()
{
    Function *func;
    BasicBlock *start_bb,*then_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    start_bb = new BasicBlock(func);
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    
    start_bb->addPred(builder->getInsertBB());
    builder->getInsertBB()->addSucc(start_bb);
    new UncondBrInstruction(start_bb, builder->getInsertBB());
    builder->setInsertBB(start_bb);

    cond->genCode();
    backPatch(cond->trueList(), then_bb);
    backPatchF(cond->falseList(), end_bb);
    
    then_bb->addPred(builder->getInsertBB());
    end_bb->addPred(builder->getInsertBB());
    builder->getInsertBB()->addSucc(then_bb);
    builder->getInsertBB()->addSucc(end_bb);
    
    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(start_bb, then_bb);

    builder->setInsertBB(end_bb);
}




void CompoundStmt::genCode()
{
    // Todo
    stmt->genCode();
}

void SeqNode::genCode()
{
    // Todo
    stmt1->genCode();
    stmt2->genCode();
}

void DeclStmt::genCode()
{
   varDefs->genCode();
}

void VarDefs::genCode()
{
    VarDefs1->genCode();
    VarDefs2->genCode();
}
void VarDef::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(rid->getSymPtr());
    if(se->isGlobal())
    {
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        fprintf(yyout, "  %s = global %s 0, align 4\n", se->toStr().c_str(), se->getType()->toStr().c_str());
    }
    else if(se->isLocal())
    {
        Function *func = builder->getInsertBB()->getParent();
        BasicBlock *entry = func->getEntry();
        Instruction *alloca;
        Operand *addr;
        SymbolEntry *addr_se;
        Type *type;
        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
        entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);                                          // set the addr operand in symbol entry so that we can use it in subsequent code generation.
    }
}
void VarInit::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(rid->getSymPtr());
    if(se->isGlobal())
    {
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
    }
    else if(se->isLocal())
    {
        Function *func = builder->getInsertBB()->getParent();
        BasicBlock *entry = func->getEntry();
        Instruction *alloca;
        Operand *addr;
        SymbolEntry *addr_se;
        Type *type;
        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
        entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);                                          // set the addr operand in symbol entry so that we can use it in subsequent code generation.
    }
    BasicBlock *bb = builder->getInsertBB();
    expr->genCode();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(rid->getSymPtr())->getAddr();
    Operand *src = expr->getOperand();
    new StoreInstruction(addr, src, bb);
}

void ConstDecl::genCode()
{
   constDefs->genCode();
}
void ConstDefs::genCode()
{
    ConstDefs1->genCode();
    ConstDefs2->genCode();
}
void ConstDef::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(rid->getSymPtr());
    if(se->isGlobal())
    {
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
    }
    else if(se->isLocal())
    {
        Function *func = builder->getInsertBB()->getParent();
        BasicBlock *entry = func->getEntry();
        Instruction *alloca;
        Operand *addr;
        SymbolEntry *addr_se;
        Type *type;
        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
        entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);                                          // set the addr operand in symbol entry so that we can use it in subsequent code generation.
    }
    BasicBlock *bb = builder->getInsertBB();
    expr->genCode();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(rid->getSymPtr())->getAddr();
    Operand *src = expr->getOperand();
    new StoreInstruction(addr, src, bb);
}

void ReturnStmt::genCode()
{
    // Todo
    BasicBlock *bb = builder->getInsertBB();
    if(retValue!=nullptr)
    {
        retValue->genCode();
        Operand *src = retValue->getOperand();
        new RetInstruction(src, bb);
    }
    else
    {
        new RetInstruction(nullptr, bb);
    }
}

void AssignStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    expr->genCode();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymPtr())->getAddr();
    Operand *src = expr->getOperand();
    /***
     * We haven't implemented array yet, the lval can only be ID. So we just store the result of the `expr` to the addr of the id.
     * If you want to implement array, you have to caculate the address first and then store the result into it.
     */
    new StoreInstruction(addr, src, bb);
}

void FuncFParams::genCode()
{
    FuncFParams1->genCode(); 
    FuncFParams2->genCode();
}
void FuncFParams::print()
{
    FuncFParams1->print(); 
    FuncFParams2->print();
}
void FuncFParam::genCode()
{
    Operand *addr;
    SymbolEntry *addr_se;
    addr_se = new IdentifierSymbolEntry(*dynamic_cast<IdentifierSymbolEntry *>(se));
    addr_se->setType(se->getType());
    addr = new Operand(addr_se);
    dynamic_cast<IdentifierSymbolEntry *>(se)->setAddr(addr);
    //assert(addr==nullptr);
}
void FuncFParam::print()
{
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%s %s ", 
        type.c_str(), name.c_str());
}

void Ast::typeCheck()
{
    if(root != nullptr)
        root->typeCheck();
}

void FunctionDef::typeCheck()
{
    // Todo
}

void BinaryExpr::typeCheck()
{
    // Todo
}

void Constant::typeCheck()
{
    // Todo
}

void Id::typeCheck()
{
    // Todo
}

void IfStmt::typeCheck()
{
    // Todo
}

void IfElseStmt::typeCheck()
{
    // Todo
}

//new added
void WhileStmt::typeCheck()
{
    // Todo
}

void CompoundStmt::typeCheck()
{
    // Todo
}

void SeqNode::typeCheck()
{
    // Todo
}

void DeclStmt::typeCheck()
{
    // Todo
}

void ReturnStmt::typeCheck()
{
    // Todo
}

void AssignStmt::typeCheck()
{
    // Todo
}

void BinaryExpr::output(int level)
{
    std::string op_str;
    switch(op)
    {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
        case AND:
            op_str = "and";
            break;
        case OR:
            op_str = "or";
            break;
        case EQ:
            op_str = "eq";
            break;
        case LESS:
            op_str = "less";
            break;
        case GREATER:
            op_str = "greater";
            break;
        case MUL:
            op_str = "mul";
            break;
        case DIV:
            op_str = "div";
            break;
        case MOD:
            op_str = "mod";
            break;
        case NEQ:
            op_str = "neq";
            break;
        case GEQ:
            op_str = "geq";
            break;
        case LEQ:
            op_str = "leq";
            break;

    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

void UnaryExpr::output(int level)
{
    std::string op_str;
    switch(op)
    {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
        case NOT:
            op_str = "not";
            break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr->output(level + 4);
}

void Ast::output()
{
    fprintf(yyout, "program\n");
    if(root != nullptr)
        root->output(4);
}

void Constant::output(int level)
{
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

void Id::output(int level)
{
    std::string name, type;
    int scope, isConst, isArray;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    isConst = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getIsConst();
    isArray = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getIsArray();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\tisConst: %d\tisArray: %d\n", level, ' ',
            name.c_str(), scope, type.c_str(), isConst, isArray);
    if(isArray&&isLval)
    {
        fprintf(yyout, "%*cArrayStruct\n", level+4, ' ');
        this->expr->output(level+4);
    }

}
void Id::setArrayStruct(ExprNode* expr)
{
    this->expr=expr;
}
void Id::setIsLval(bool isLval)
{
    this->isLval=isLval;
}
void ArrayStruct::output(int level)
{
    fprintf(yyout, "%*cArrayStruct\n", level, ' ');
    expr1->output(level + 4);
    expr2->output(level + 4);
}

void ArrayVal::output(int level)
{
    fprintf(yyout, "%*cArrayVal\n", level, ' ');
    expr1->output(level + 4);
    expr2->output(level + 4);
}

void ArrayInit::output(int level)
{
    if(isArray)
    {
        fprintf(yyout, "%*cArrayElement\n", level, ' ');
        expr->output(level + 4);
    }
    else
    {
        expr->output(level);
    }
}

void ArrayInits::output(int level)
{
    fprintf(yyout, "%*cArrayInits\n", level, ' ');
    expr1->output(level + 4);
    expr2->output(level + 4);
}
char* StmtNode::getId()
{
    return NULL;
}
int StmtNode::isLeaf()
{
    return -1;
}
StmtNode* StmtNode::getV1()
{
    return NULL;
}
StmtNode* StmtNode::getV2()
{
    return NULL;
}
StmtNode* StmtNode::getC1()
{
    return NULL;
}
StmtNode* StmtNode::getC2()
{
    return NULL;
}
void StmtNode::setId(Id* id)
{
    return ;
}
void StmtNode::markArray(bool array)
{
    return ;
}
void StmtNode::setConstNum(ExprNode* num)
{
    return ;
}
bool StmtNode::getIsArray()
{
    return 0;
}


void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    stmt->output(level + 4);
}

void SeqNode::output(int level)
{
    stmt1->output(level);
    stmt2->output(level);
}

void DeclStmt::output(int level)
{
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    varDefs->output(level + 4);
}

void ConstDecl::output(int level)
{
    fprintf(yyout, "%*cConstDecl\n", level, ' ');
    constDefs->output(level + 4);
}

void VarDefs::output(int level)
{
    fprintf(yyout, "%*cVarDefs\n", level, ' ');
    VarDefs1->output(level + 4);
    VarDefs2->output(level + 4);
}

void ConstDefs::output(int level)
{
    fprintf(yyout, "%*cConstDefs\n", level, ' ');
    ConstDefs1->output(level + 4);
    ConstDefs2->output(level + 4);
}

void VarDef::output(int level)
{
    fprintf(yyout, "%*cVarDef\n", level, ' ');
    rid->output(level + 4);
    if(isArray)
    {
        constNum->output(level+4);
    }
}

void VarInit::output(int level)
{
    fprintf(yyout, "%*cVarInit\n", level, ' ');
    rid->output(level + 4);
    if(isArray)
    {
        constNum->output(level+4);
    }
    expr->output(level + 4);
}

void ConstDef::output(int level)
{
    fprintf(yyout, "%*cConstDef\n", level, ' ');
    rid->output(level + 4);
    if(isArray)
    {
        constNum->output(level+4);
    }
    expr->output(level + 4);
}

void IfStmt::output(int level)
{
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

//new added
void WhileStmt::output(int level)
{
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void IfElseStmt::output(int level)
{
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}

void ReturnStmt::output(int level)
{
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    if(retValue==nullptr)
        fprintf(yyout, "%*cReturn Null\n", level+4, ' ');
    else
        retValue->output(level + 4);

}

void AssignStmt::output(int level)
{
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void ExpStmt::output(int level)
{
    fprintf(yyout, "%*cExpStmt\n", level, ' ');
    expr->output(level + 4);
}
void EmptyStmt::output(int level)
{
    fprintf(yyout, "%*cEmptyStmt\n", level, ' ');
}

void EmptyBlock::output(int level)
{
    fprintf(yyout, "%*cEmptyBlock\n", level, ' ');
}

void FunctionDef::output(int level)
{
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ', 
            name.c_str(), type.c_str());
    
    if(params!=NULL){
    	params->output(level + 4);
	}
	
    
    stmt->output(level + 4);

}

void BreakStmt::output(int level)
{
    fprintf(yyout, "%*cBreakStmt\n", level, ' ');
    
}

void ContinueStmt::output(int level)
{
    fprintf(yyout, "%*cContinueStmt\n", level, ' ');
    
}

void FuncFParams::output(int level)
{
    fprintf(yyout, "%*cFuncFParams\n", level, ' ');
    FuncFParams1->output(level + 4);
    FuncFParams2->output(level + 4);
}


void FuncFParam::output(int level)
{
    
    std::string name, type;
    int scope, isConst, isArray;
    name = se->toStr();
    type = se->getType()->toStr();
    isConst = dynamic_cast<IdentifierSymbolEntry*>(se)->getIsConst();
    isArray = dynamic_cast<IdentifierSymbolEntry*>(se)->getIsArray();
    scope = dynamic_cast<IdentifierSymbolEntry*>(se)->getScope();
    fprintf(yyout, "%*cFuncFParam\tname: %s\tscope: %d\ttype: %s\tisConst: %d\tisArray: %d\n", level, ' ',
            name.c_str(), scope, type.c_str(), isConst, isArray);
    
    if(isArray && constNum)
    {
        constNum->output(level+4);
    }

}

void LeafFunc::output(int level)
{
    
    fprintf(yyout, "%*cLeafFunction function name: %s\n", level, ' ', id);
    if(params!=NULL){
    	params->output(level + 4);
	}
    
}

void FuncRParams::output(int level)
{
    fprintf(yyout, "%*cFuncRParams\n", level, ' ');
    Exp1->output(level + 4);
    Exp2->output(level + 4);
}
