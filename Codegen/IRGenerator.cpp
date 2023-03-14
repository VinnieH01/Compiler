#include "IRGenerator.h"
#include <llvm/IR/Verifier.h>
#include <functional>
#include <stdexcept>

using namespace nlohmann;
using namespace llvm;

IRGenerator::IRGenerator(const std::unique_ptr<LLVMContext>& ctx, const std::unique_ptr<Module>& mdl, const std::unique_ptr<IRBuilder<>>& bldr, const std::unique_ptr<legacy::FunctionPassManager>& fpm)
    : context(ctx), module(mdl), builder(bldr), function_pass_manager(fpm)
{}

Value* IRGenerator::visit_node(const json& data)
{
    static std::map<std::string, std::function<Value* (const json&)>> dispatch
    {
        { "Function", [this](const json& j) { return visit_function_node(j); } },
        { "Prototype", [this](const json& j) { return visit_prototype_node(j); } },
        { "Binary", [this](const json& j) { return visit_binary_node(j); } },
        { "Number", [this](const json& j) { return visit_number_node(j); } },
        { "Variable", [this](const json& j) { return visit_variable_node(j); } },
        { "Call", [this](const json& j) { return visit_call_node(j); } },
        { "Unary", [this](const json& j) { return visit_unary_node(j); } },
        { "Return", [this](const json& j) { return visit_return_node(j); } },
        { "Let", [this](const json& j) { return visit_let_node(j); } },
    };

    return dispatch.at((data["type"]))(data);
}

Value* IRGenerator::visit_number_node(const json& data)
{
    return ConstantFP::get(*context, APFloat((double)data["value"]));
}

Value* IRGenerator::visit_variable_node(const json& data)
{
    std::string var_name = data["name"];

    if (!named_values.count(var_name)) 
        throw std::runtime_error("Variable does not exist: " + var_name);

    AllocaInst* A = named_values[var_name];
    return builder->CreateLoad(A->getAllocatedType(), A, std::string(data["name"]));
}

Value* IRGenerator::visit_let_node(const json& data)
{
    Function* function = builder->GetInsertBlock()->getParent();
    AllocaInst* alloca_inst = create_alloca_at_top(function, data["name"]);

    Value* value = visit_node(data["value"]);

    std::string var_name = data["name"];

    if (named_values.count(var_name))
        throw std::runtime_error("Cannot redeclare variable: " + var_name);

    named_values[data["name"]] = alloca_inst;
    return builder->CreateStore(value, alloca_inst);
}

Value* IRGenerator::visit_binary_node(const json& data)
{
    std::string operation = data["operator"];

    if (operation == ":=")
    {
        Value* val = visit_node(data["right"]);
        Value* variable = named_values[data["left"]["name"]];

        return builder->CreateStore(val, variable);
    }

    Value* lhs = visit_node(data["left"]);
    Value* rhs = visit_node(data["right"]);

    if (operation == "+")
    {
        return builder->CreateFAdd(lhs, rhs, "add");
    }
    if (operation == "-")
    {
        return builder->CreateFSub(lhs, rhs, "sub");
    }
    if (operation == "*")
    {
        return builder->CreateFMul(lhs, rhs, "mul");
    }
    if (operation == "<" || operation == ">" || operation == "=")
    {
        if (operation == "<")
            lhs = builder->CreateFCmpULT(lhs, rhs, "cmplt");
        else if (operation == ">")
            lhs = builder->CreateFCmpUGT(lhs, rhs, "cmpgt");
        else if (operation == "=")
            lhs = builder->CreateFCmpUEQ(lhs, rhs, "cmpeq");

        // Convert bool 0/1 to double 0.0 or 1.0
        return builder->CreateUIToFP(lhs, Type::getDoubleTy(*context), "bool");
    }

    throw std::runtime_error("Unknown operator in AST " + operation);
}

Value* IRGenerator::visit_unary_node(const json& data)
{
    std::string operation = data["operator"];

    Value* zero = ConstantFP::get(*context, APFloat(0.0));
    Value* operand = visit_node(data["operand"]);

    if (operation == "+")
    {
        return builder->CreateFAdd(zero, operand, "addtmp");
    }
    if (operation == "-")
    {
        return builder->CreateFSub(zero, operand, "subtmp");
    }

    throw std::runtime_error("Unknown operator in AST " + operation);
}

Value* IRGenerator::visit_return_node(const json& data)
{
    Value* expr_val = visit_node(data["value"]);
    return builder->CreateRet(expr_val);
}

Function* IRGenerator::visit_prototype_node(const json& data)
{
    std::vector<std::string> args = data["args"];
    std::string name = data["name"];

    //Create a list of double types (1 for every argument)
    std::vector<Type*> arg_types(args.size(), Type::getDoubleTy(*context));
    Type* return_type = data["ret_type"] == "void" ? Type::getVoidTy(*context) : Type::getDoubleTy(*context);
    FunctionType* func_type = FunctionType::get(return_type, arg_types, false);

    Function* function = Function::Create(func_type, Function::ExternalLinkage, name, module.get());

    // Set names for all arguments.
    size_t idx = 0;
    for (Argument& arg : function->args()) arg.setName(args[idx++]);

    return function;
}

Value* IRGenerator::visit_function_node(const json& data)
{
    json prototype = data["prototype"];

    //If function is already declared we dont want to redeclare
    Function* function = module->getFunction((std::string)prototype["name"]);

    if (!function)
    {
        function = visit_prototype_node(prototype);
    }

    BasicBlock* function_block = BasicBlock::Create(*context, "entry", function);
    builder->SetInsertPoint(function_block);

    // Record the function arguments in the named_values map.
    named_values.clear();
    for (Argument& arg : function->args()) 
    {
        //Create an alloca for this variable at the start of the function
        AllocaInst* alloca_inst = create_alloca_at_top(function, arg.getName().str());

        //Store the initial value in the alloca
        builder->CreateStore(&arg, alloca_inst);

        named_values[std::string(arg.getName())] = alloca_inst;
    }

    json body_data = data["body"];
    for (const json& data : body_data)
    {
        visit_node(data);
    }

    if (function->getReturnType()->isVoidTy())
    {
        //Return at the end of void function
        builder->CreateRetVoid();
    }

    bool fail = verifyFunction(*function);

    if (fail) throw std::runtime_error("Function verification failed: " + prototype["name"]);

    //Optimize function
    function_pass_manager->run(*function);

    return function;
}

Value* IRGenerator::visit_call_node(const json& data)
{
    // Look up the name in the global module table.
    Function* callee = module->getFunction((std::string)data["callee"]);

    if (!callee) throw std::runtime_error("Function does not exist: " + data["callee"]);

    std::vector<json> args_data = data["args"];

    std::vector<Value*> arg_values;
    arg_values.reserve(args_data.size());
    for (size_t i = 0; i < args_data.size(); ++i) 
    {
        arg_values.push_back(visit_node(args_data[i]));
    }

    //Cannot give void type a name 
    if (callee->getReturnType()->isVoidTy()) return builder->CreateCall(callee, arg_values);
    return builder->CreateCall(callee, arg_values, "fnret");
}

AllocaInst* IRGenerator::create_alloca_at_top(Function* func, const std::string& variable_name) {
    static llvm::IRBuilder<> entry_builder(&func->getEntryBlock(), func->getEntryBlock().begin());

    entry_builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    return entry_builder.CreateAlloca(llvm::Type::getDoubleTy(*context), nullptr, variable_name);
}