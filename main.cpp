#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Bitcode/BitcodeWriter.h>

#include "json.hpp"

using namespace llvm;
using json = nlohmann::json;

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, AllocaInst*> NamedValues;

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static AllocaInst* create_entry_block_alloca(Function* TheFunction,
    const std::string& VarName) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
        TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type::getDoubleTy(*TheContext), nullptr,
        VarName);
}

Value* visit_node(const json&);

Value* visit_number_node(const json& data) 
{
    return ConstantFP::get(*TheContext, APFloat((double)data["value"]));
}

Value* visit_variable_node(const json& data) 
{
    AllocaInst* A = NamedValues[data["name"]];
    return Builder->CreateLoad(A->getAllocatedType(), A, std::string(data["name"]));
}

Value* visit_let_node(const json& data) 
{
    Function* function = Builder->GetInsertBlock()->getParent();
    AllocaInst* Alloca = create_entry_block_alloca(function, data["name"]);
    Value* value = visit_node(data["value"]);
    NamedValues[data["name"]] = Alloca;
    return Builder->CreateStore(value, Alloca);
}

Value* visit_binary_node(const json& data) 
{
    std::string operation = data["operator"];

    if (operation == ":=")
    {
        Value* val = visit_node(data["right"]);
        Value* variable = NamedValues[data["left"]["name"]];

        return Builder->CreateStore(val, variable);
    }

    Value* L = visit_node(data["left"]);
    Value* R = visit_node(data["right"]);

    if (operation == "+") 
    {
        return Builder->CreateFAdd(L, R, "addtmp");
    }
    if (operation == "-") 
    {
        return Builder->CreateFSub(L, R, "subtmp");
    }
    if (operation == "*") 
    {
        return Builder->CreateFMul(L, R, "subtmp");
    }
    if (operation == "<" || operation == ">" || operation == "=")
    {
        if(operation == "<")
            L = Builder->CreateFCmpULT(L, R, "cmptmp");
        else if(operation == ">")
            L = Builder->CreateFCmpUGT(L, R, "cmptmp");
        else if (operation == "=")
            L = Builder->CreateFCmpUEQ(L, R, "cmptmp");

        // Convert bool 0/1 to double 0.0 or 1.0
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    }
}

Value* visit_unary_node(const json& data) 
{
    Value* operand = visit_node(data["operand"]);

    std::string operation = data["operator"];

    Value* zero = ConstantFP::get(*TheContext, APFloat(0.0));

    if (operation == "+")
    {
        return Builder->CreateFAdd(zero, operand, "addtmp");
    }
    if (operation == "-")
    {
        return Builder->CreateFSub(zero, operand, "subtmp");
    }
}

Value* visit_return_node(const json& data)
{
    Value* expr_val = visit_node(data["value"]);
    return Builder->CreateRet(expr_val);
}

Function* visit_prototype_node(const json& data) 
{
    std::vector<std::string> args = data["args"];
    std::string name = data["name"];

    // Make the function type:  double(double,double) etc.
    std::vector<Type*> Doubles(args.size(), Type::getDoubleTy(*TheContext));
    
    //Currently just a "hack" so the main function can be declared as an action (void return)
    Type* return_type = name == "main" ? Type::getInt32Ty(*TheContext)
        : data["ret_type"] == "void" ?
        Type::getVoidTy(*TheContext) 
        : Type::getDoubleTy(*TheContext);
    FunctionType* FT = FunctionType::get(return_type, Doubles, false);

    Function* F = Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());

    // Set names for all arguments.
    unsigned Idx = 0;
    for (auto& Arg : F->args())
        Arg.setName(args[Idx++]);

    return F;
}

Value* visit_function_node(const json& data)
{
    json prototype = data["prototype"];
	
	//If function is already declared we dont want to redeclare
    Function* function = TheModule->getFunction((std::string)prototype["name"]);

	if(!function) 
    {
        function = visit_prototype_node(prototype);
    }

    BasicBlock* BB = BasicBlock::Create(*TheContext, "entry", function);
    Builder->SetInsertPoint(BB);
	
    // Record the function arguments in the NamedValues map.
    NamedValues.clear();
    for (auto& Arg : function->args()) {
        //Create an alloca for this variable at the start of the function
        AllocaInst* Alloca = create_entry_block_alloca(function, Arg.getName().str());

        //Store the initial value in the alloca
        Builder->CreateStore(&Arg, Alloca);

        NamedValues[std::string(Arg.getName())] = Alloca;
    }

    json body_data = data["body"];
    for (const json& data : body_data)
    {
        visit_node(data);
    }

    if (function->getName() == "main") 
    {
        //Currently just a "hack" so the main function can be declared as an action (void return)
        Builder->CreateRet(Constant::getIntegerValue(Type::getInt32Ty(*TheContext), APInt(32, 0)));
    }
    else if(function->getReturnType()->isVoidTy()) 
    {
        //Return at the end of void function
        Builder->CreateRetVoid();
    }

    verifyFunction(*function);

    return function;
}

Value* visit_call_node(const json& data) 
{
    std::string callee = data["callee"];

    // Look up the name in the global module table.
    Function* CalleeF = TheModule->getFunction(callee);

    std::vector<json> args_data = data["args"];

    std::vector<Value*> ArgsV;
    for (unsigned i = 0, e = args_data.size(); i != e; ++i) {
        ArgsV.push_back(visit_node(args_data[i]));
        if (!ArgsV.back())
            return nullptr;
    }

    //Cannot give void type a name 
    if (CalleeF->getReturnType()->isVoidTy()) return Builder->CreateCall(CalleeF, ArgsV);
    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Value* visit_node(const json& data) 
{
    std::string type = data["type"];

    if (type == "Function") 
    {
        return visit_function_node(data);
    }
    if (type == "Prototype")
    {
        return visit_prototype_node(data);
    }
    if (type == "Binary") 
    {
        return visit_binary_node(data);
    }
    if (type == "Number") 
    {
        return visit_number_node(data);
    }
    if (type == "Variable") 
    {
        return visit_variable_node(data);
    }
    if (type == "Call") 
    {
        return visit_call_node(data);
    }
    if(type == "Unary") 
    {
        return visit_unary_node(data);
    }
    if (type == "Return") 
    {
        return visit_return_node(data);
    }
    if (type == "Let") 
    {
        return visit_let_node(data);
    }
}

int main(int argc, char* argv[])
{
    if (argc != 2) {
        std::cout << "Incorrect # of arguments";
        return -1;
    }

    std::ifstream f(argv[1]);
    std::vector<json> data = json::parse(f);

    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("Module", *TheContext);
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    for (const json& data0 : data)
    {
        visit_node(data0);
    }


	TheModule->print(outs(), nullptr);

    std::error_code EC;
    llvm::raw_fd_ostream OS("module", EC);
    WriteBitcodeToFile(*TheModule, OS);
    OS.flush();

    // Make the function type:  double(double,double) etc.
    /*std::vector<Type*> Doubles(5, Type::getDoubleTy(*TheContext));
    FunctionType* FT =
        FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

    Function* F =
        Function::Create(FT, Function::ExternalLinkage, "foo", TheModule.get());

    BasicBlock* BB = BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);

    Value* val = Builder->CreateFAdd(ConstantFP::get(*TheContext, APFloat(2.0)), ConstantFP::get(*TheContext, APFloat(5.0)), "bob");
    Builder->CreateRet(val);


    TheModule->print(errs(), nullptr);*/
	
    return 0;
}