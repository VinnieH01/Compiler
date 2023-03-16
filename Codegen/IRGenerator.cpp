#include "IRGenerator.h"
#include <llvm/IR/Verifier.h>
#include <functional>
#include <stdexcept>

using namespace nlohmann;
using namespace llvm;

IRGenerator::IRGenerator(const std::unique_ptr<LLVMContext>& ctx, const std::unique_ptr<Module>& mdl, const std::unique_ptr<IRBuilder<>>& bldr, const std::unique_ptr<legacy::FunctionPassManager>& fpm)
    : context(ctx), module(mdl), builder(bldr), function_pass_manager(fpm), in_function(false)
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
        { "If", [this](const json& j) { return visit_if_node(j); }},
        { "Loop", [this](const json& j) { return visit_loop_node(j); }},
        { "LoopTermination", [this](const json& j) { return visit_loop_termination_node(j); }},
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

    GlobalVariable* g_var = module->getNamedGlobal((std::string)data["name"]);

    //Global variable takes precedence over local
    if(g_var) 
    {
        return builder->CreateLoad(g_var->getValueType(), g_var, std::string(data["name"]));
    }

    if (!named_values.count(var_name)) 
        throw std::runtime_error("Variable does not exist: " + var_name);

    AllocaInst* A = named_values[var_name];
    return builder->CreateLoad(A->getAllocatedType(), A, std::string(data["name"]));
}

Value* IRGenerator::visit_let_node(const json& data)
{
    Value* value = visit_node(data["value"]);

    if (!in_function)
    {
        //We are outside of a function and should declare variable globally.

        //TODO: Cast to Constant* is scary.. for now its fine because the parser disallows anything else
        return create_global_variable(data["name"], (Constant*)value);
    }
    else
    {
        Function* function = builder->GetInsertBlock()->getParent();

        AllocaInst* alloca_inst = create_alloca_at_top(function, data["name"]);

        std::string var_name = data["name"];

        if (named_values.count(var_name))
            throw std::runtime_error("Cannot redeclare variable: " + var_name);

        named_values[data["name"]] = alloca_inst;
        return builder->CreateStore(value, alloca_inst);
    }
}

Value* IRGenerator::visit_binary_node(const json& data)
{
    std::string operation = data["operator"];

    if (operation == ":=")
    {
        Value* val = visit_node(data["right"]);

        GlobalVariable* g_var = module->getNamedGlobal((std::string)data["left"]["name"]);

        //Global variable takes precedence over local
        if (g_var)
        {
            return builder->CreateStore(val, g_var);
        }
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

    in_function = true;

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

    bool fail = verifyFunction(*function, &outs());

    if (fail) throw std::runtime_error("Function verification failed");

    //Optimize function
    function_pass_manager->run(*function);

    in_function = false;

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

Value* IRGenerator::visit_if_node(const json& data)
{
    Value* condition = visit_node(data["condition"]);

    // Convert condition to a bool so 0.0 becomes false
    condition = builder->CreateFCmpONE(condition, ConstantFP::get(*context, APFloat(0.0)), "ifcond");

    Function* function = builder->GetInsertBlock()->getParent();

    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function (Where we are, as it goes top to bottom).
    BasicBlock* then_block = BasicBlock::Create(*context, "then", function);
    BasicBlock* else_block = BasicBlock::Create(*context, "else");
    BasicBlock* continue_block = BasicBlock::Create(*context, "ifcont");

    builder->CreateCondBr(condition, then_block, else_block);

    builder->SetInsertPoint(then_block);

    json then_data = data["then"];
    bool terminate_in_block = false;
    for (const json& data : then_data)
    {
        //If we find a return or break we want to break out of this loop since nothing can run after the return/break anyway.
        //Without this the verifier would complain and the function would be invalid.
        visit_node(data);
        if (data["type"] == "Return" || data["type"] == "LoopTermination")
        {
            terminate_in_block = true;
            break;
        }
    }

    //We can only have one terminator
    if(!terminate_in_block)
    {
        //Branch to continue after then is done
        builder->CreateBr(continue_block);
    }

    //Insert else at end of function
    else_block->insertInto(function);

    builder->SetInsertPoint(else_block);

    json else_data = data["else"];
    terminate_in_block = false;
    for (const json& data : else_data)
    {
        visit_node(data);
        if (data["type"] == "Return" || data["type"] == "LoopTermination")
        {
            terminate_in_block = true;
            break;
        }
    }

    //We can only have one terminator
    if (!terminate_in_block)
    {
        //Branch to continue after then is done
        builder->CreateBr(continue_block);
    }

    //Add the continue block at the end
    continue_block->insertInto(function);
    builder->SetInsertPoint(continue_block);

    //TODO: What to return here...?
    return then_block;
}

Value* IRGenerator::visit_loop_node(const json& data)
{
    Function* function = builder->GetInsertBlock()->getParent();

    // Create blocks for the loop and continue cases. Insert the 'loop' block at the
    // end of the function (Where we are, as it goes top to bottom).
    BasicBlock* loop_block = BasicBlock::Create(*context, "loop", function);
    BasicBlock* continuation_block = BasicBlock::Create(*context, "loopcont");

    builder->CreateBr(loop_block);
    builder->SetInsertPoint(loop_block);

    json body_data = data["body"];
    bool terminate_in_block = false;
    for (const json& data : body_data)
    {
        //We have to set this in the loop since an inner loop node could change it.
        //Also NOTE: "loop_continue_block" refers to the continue keyword. Not the continuation block
        loop_break_block = continuation_block;
        loop_continue_block = loop_block;

        //If we find a "return" or "break" we want to break out of this loop since nothing can run after the return/break anyway.
        //Without this the verifier would complain and the function would be invalid.
        visit_node(data);
        if (data["type"] == "Return" || data["type"] == "LoopTermination")
        {
            terminate_in_block = true;
            break;
        }
    }

    loop_break_block = nullptr;
    loop_continue_block = nullptr;

    //We can only have one terminator
    //Normally this bool would not be set since the code "loop {...; break; ...;}" is unnecessary use of a loop... but it is valid so we need to check for it.
    if (!terminate_in_block)
    {
        //Branch back to the top
        builder->CreateBr(loop_block);
    }

    //Add the continue block at the end
    continuation_block->insertInto(function);
    builder->SetInsertPoint(continuation_block);

    //TODO: What to return here...?
    return loop_block;
}

Value* IRGenerator::visit_loop_termination_node(const json& data)
{
    if (!loop_break_block) //Dont need to check continue as well, if one is set the other should be as well.
        throw std::runtime_error("Cannot break/continue outside loop");

    BasicBlock* branch_to = data["break"] ? loop_break_block : loop_continue_block;

    return builder->CreateBr(branch_to);
}

AllocaInst* IRGenerator::create_alloca_at_top(Function* func, const std::string& variable_name) {
    static llvm::IRBuilder<> entry_builder(&func->getEntryBlock(), func->getEntryBlock().begin());

    entry_builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    return entry_builder.CreateAlloca(llvm::Type::getDoubleTy(*context), nullptr, variable_name);
}

GlobalVariable* IRGenerator::create_global_variable(const std::string& variable_name, Constant* init_val)
{
    if(module->getNamedGlobal(variable_name)) 
        throw std::runtime_error("Cannot redefine global variable: " + variable_name);

    module->getOrInsertGlobal(variable_name, Type::getDoubleTy(*context));
    GlobalVariable* global_variable = module->getNamedGlobal(variable_name);
    global_variable->setLinkage(GlobalValue::ExternalLinkage);
    global_variable->setInitializer(init_val);
    return global_variable;
}
