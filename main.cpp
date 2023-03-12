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
static std::map<std::string, Value*> NamedValues;

Value* visit_node(const json&);

Value* visit_number_node(const json& data) 
{
    return ConstantFP::get(*TheContext, APFloat((double)data["value"]));
}

Value* visit_variable_node(const json& data) 
{
    Value* V = NamedValues[data["name"]];
    return V;
}

Value* visit_binary_node(const json& data) 
{
    Value* L = visit_node(data["left"]);
    Value* R = visit_node(data["right"]);

    std::string operation = data["operator"];

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

Function* visit_prototype_node(const json& data) 
{
    std::vector<std::string> args = data["args"];
    std::string name = data["name"];

    // Make the function type:  double(double,double) etc.
    std::vector<Type*> Doubles(args.size(), Type::getDoubleTy(*TheContext));
    FunctionType* FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

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
    for (auto& Arg : function->args())
        NamedValues[std::string(Arg.getName())] = &Arg;

    if (Value* RetVal = visit_node(data["body"])) {
        // Finish off the function.
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency.
        verifyFunction(*function);

        return function;
    }
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


	//TheModule->print(outs(), nullptr);

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