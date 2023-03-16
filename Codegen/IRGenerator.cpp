#include "IRGenerator.h"
#include <llvm/IR/Verifier.h>
#include <functional>
#include <stdexcept>

using namespace nlohmann;
using namespace llvm;

IRGenerator::IRGenerator(const std::unique_ptr<LLVMContext>& ctx, const std::unique_ptr<Module>& mdl, const std::unique_ptr<IRBuilder<>>& bldr, const std::unique_ptr<legacy::FunctionPassManager>& fpm)
    : 
    context(ctx), 
    module(mdl), 
    builder(bldr),
    function_pass_manager(fpm), 
    in_function(false),
    loop_break_block(nullptr), 
    loop_continue_block(nullptr)
{
    type_names =
    {
        {"void", Type::getVoidTy(*context)},
        {"i32", Type::getInt32Ty(*context)},
        {"f64", Type::getDoubleTy(*context)}
    };
}

Value* IRGenerator::visit_node(const json& data)
{
    static std::map<std::string, std::function<Value* (const json&)>> dispatch
    {
        { "Function", [this](const json& j) { return visit_function_node(j); } },
        { "Prototype", [this](const json& j) { return visit_prototype_node(j); } },
        { "Binary", [this](const json& j) { return visit_binary_node(j); } },
        { "Literal", [this](const json& j) { return visit_literal_node(j); } },
        { "Variable", [this](const json& j) { return visit_variable_node(j); } },
        { "Call", [this](const json& j) { return visit_call_node(j); } },
        { "Unary", [this](const json& j) { return visit_unary_node(j); } },
        { "Return", [this](const json& j) { return visit_return_node(j); } },
        { "Let", [this](const json& j) { return visit_let_node(j); } },
        { "If", [this](const json& j) { return visit_if_node(j); }},
        { "Loop", [this](const json& j) { return visit_loop_node(j); }},
        { "LoopTermination", [this](const json& j) { return visit_loop_termination_node(j); }},
    };

    std::function<Value* (const json&)> func = nullptr;
    try { func = dispatch.at((data["type"])); }
    catch (std::exception e) { outs() << "Unknown node type in AST: " + (std::string)data["type"]; }

    return func(data);
}

Value* IRGenerator::visit_literal_node(const json& data)
{
    if (data["data_type"] == "i32") 
    {
        return ConstantInt::get(*context, APInt(32, (uint64_t)data["value"], true));
    }
    else if(data["data_type"] == "f64") 
    {
        return ConstantFP::get(*context, APFloat((double)data["value"]));
    }

    outs() << "Literal with datatype not supported: " + (std::string)data["name"] + " " + (std::string)data["data_type"];

    return nullptr;
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
        outs() << ("Variable does not exist: " + (std::string)var_name);

    AllocaInst* A = named_values[var_name];
    return builder->CreateLoad(A->getAllocatedType(), A, std::string(data["name"]));
}

Value* IRGenerator::visit_let_node(const json& data)
{
    Value* value = visit_node(data["value"]);
    Type* type = type_names.at(data["data_type"]);

    if (type != value->getType())
    {
        outs() << "Incompatible types in let assignment: " + (std::string)data["name"];
        return nullptr;
    }

    if (!in_function)
    {
        //We are outside of a function and should declare variable globally.

        //TODO: Cast to Constant* is scary.. for now its fine because the parser disallows anything else
        return create_global_variable(data["name"], type, (Constant*)value);
    }
    else
    {
        Function* function = builder->GetInsertBlock()->getParent();

        AllocaInst* alloca_inst = create_alloca_at_top(function, data["name"], type);

        std::string var_name = data["name"];

        if (named_values.count(var_name))
            outs() << ("Cannot redeclare variable: " + (std::string)var_name);

        named_values[data["name"]] = alloca_inst;

        return builder->CreateStore(value, alloca_inst);
    }
}

Value* IRGenerator::visit_binary_node(const json& data)
{
    std::string operation = data["operator"];

    if (operation == "<-")
    {
        Value* val = visit_node(data["right"]);

        GlobalVariable* g_var = module->getNamedGlobal((std::string)data["left"]["name"]);

        //Global variable takes precedence over local
        if (g_var)
        {
            if (val->getType() != g_var->getValueType()) 
            {
                outs() << "Incompatible types in assignment: " + (std::string)data["left"]["name"];
                return nullptr;
            }
            return builder->CreateStore(val, g_var);
        }

        AllocaInst* variable = named_values[data["left"]["name"]];

        if (val->getType() != variable->getAllocatedType())
        {
            outs() << "Incompatible types in assignment: " + (std::string)data["left"]["name"];
            return nullptr;
        }

        return builder->CreateStore(val, variable);
    }

    Value* lhs = visit_node(data["left"]);
    Value* rhs = visit_node(data["right"]);

    Type* type = lhs->getType();

    if(type != rhs->getType())
        outs() << "Incompatible types in binary operator";

    if (type->isDoubleTy())
    {
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
                return builder->CreateFCmpULT(lhs, rhs, "cmplt");
            else if (operation == ">")
                return builder->CreateFCmpUGT(lhs, rhs, "cmpgt");
            else if (operation == "=")
                return builder->CreateFCmpUEQ(lhs, rhs, "cmpeq");
        }

        outs() << ("Unknown operator in AST " + operation);
    }
    else if (type->isIntegerTy())
    {
        if (operation == "+")
        {
            return builder->CreateAdd(lhs, rhs, "add");
        }
        if (operation == "-")
        {
            return builder->CreateSub(lhs, rhs, "sub");
        }
        if (operation == "*")
        {
            return builder->CreateMul(lhs, rhs, "mul");
        }
        if (operation == "<" || operation == ">" || operation == "=")
        {
            if (operation == "<")
                return builder->CreateICmpULT(lhs, rhs, "cmplt");
            else if (operation == ">")
                return builder->CreateICmpUGT(lhs, rhs, "cmpgt");
            else if (operation == "=")
                return builder->CreateICmpEQ(lhs, rhs, "cmpeq");
        }

        outs() << ("Unknown operator in AST " + operation);
    }

    outs() << "Cannot perform binary operator on type";
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

    outs() << ("Unknown operator in AST " + operation);
}

Value* IRGenerator::visit_return_node(const json& data)
{
    Value* expr_val = visit_node(data["value"]);
    return builder->CreateRet(expr_val);
}

Function* IRGenerator::visit_prototype_node(const json& data)
{
    std::vector<std::string> args = data["args"];
    std::vector<std::string> arg_types_str = data["arg_types"];
    std::string name = data["name"];

    //Generate list of the Type* of each argument
    std::vector<Type*> arg_types;
    arg_types.reserve(arg_types_str.size());
    for (const std::string& str : arg_types_str) 
    {
        arg_types.push_back(type_names.at(str));
    }

    Type* return_type = type_names.at(data["ret_type"]);

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
        AllocaInst* alloca_inst = create_alloca_at_top(function, arg.getName().str(), arg.getType());

        //Store the initial value in the alloca
        builder->CreateStore(&arg, alloca_inst);

        named_values[std::string(arg.getName())] = alloca_inst;
    }

    json body_data = data["body"];
    for (const json& data : body_data)
    {
        visit_node(data);

        if (data["type"] == "Return")
        {
            //If we return there is no point in generating further instructions
            break;
        }
    }

    if (function->getReturnType()->isVoidTy())
    {
        //Return at the end of void function
        builder->CreateRetVoid();
    }

    bool fail = verifyFunction(*function, &outs());

    if (fail) outs() << ("Function verification failed" + function->getName());

    //Optimize function
    function_pass_manager->run(*function);

    in_function = false;

    return function;
}

Value* IRGenerator::visit_call_node(const json& data)
{
    // Look up the name in the global module table.
    Function* callee = module->getFunction((std::string)data["callee"]);

    if (!callee) 
    {
        outs() << ("Function does not exist: " + (std::string)data["callee"]);
        return nullptr;
    }

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

    if (!condition->getType()->isIntegerTy(1))
        outs() << "If statement requires bool type";

    Function* function = builder->GetInsertBlock()->getParent();

    // Create blocks for the then and else cases, as well as continuation which is what happens after both if blocks.
    BasicBlock* then_block = BasicBlock::Create(*context, "then");
    BasicBlock* else_block = BasicBlock::Create(*context, "else");
    BasicBlock* continuation_block = BasicBlock::Create(*context, "ifcont");

    //Branch to then if condition otherwise to else
    builder->CreateCondBr(condition, then_block, else_block);

    auto generate_block = [&](BasicBlock* block, const json& block_data)
    {
        block->insertInto(function);
        builder->SetInsertPoint(block);

        bool terminate_in_block = false;
        for (const json& data : block_data)
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

        //If we didn't terminate in the above loop we have to do so at the end of the block
        if (!terminate_in_block)
        {
            //Branch to continue after then is done
            builder->CreateBr(continuation_block);
        }
    };

    generate_block(then_block, data["then"]);
    generate_block(else_block, data["else"]);

    //Add the continue block at the end
    continuation_block->insertInto(function);
    builder->SetInsertPoint(continuation_block);

    //TODO: What to return here...?
    return then_block;
}

Value* IRGenerator::visit_loop_node(const json& data)
{
    Function* function = builder->GetInsertBlock()->getParent();

    BasicBlock* loop_block = BasicBlock::Create(*context, "loop");
    BasicBlock* continuation_block = BasicBlock::Create(*context, "loopcont");

    //We need to branch here because LLVM requires all blocks be terminated somehow. The flow cannot "fall through" into the loop block.
    builder->CreateBr(loop_block);

    loop_block->insertInto(function);
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
        outs() << ("Cannot break/continue outside loop");

    BasicBlock* branch_to = data["break"] ? loop_break_block : loop_continue_block;

    return builder->CreateBr(branch_to);
}

AllocaInst* IRGenerator::create_alloca_at_top(Function* func, const std::string& variable_name, Type* type) {
    static llvm::IRBuilder<> entry_builder(&func->getEntryBlock(), func->getEntryBlock().begin());

    entry_builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    return entry_builder.CreateAlloca(type, nullptr, variable_name);
}

GlobalVariable* IRGenerator::create_global_variable(const std::string& variable_name, Type* type, Constant* init_val)
{
    if(module->getNamedGlobal(variable_name)) 
        outs() << ("Cannot redefine global variable: " + variable_name);

    module->getOrInsertGlobal(variable_name, type);
    GlobalVariable* global_variable = module->getNamedGlobal(variable_name);
    global_variable->setLinkage(GlobalValue::ExternalLinkage);
    global_variable->setInitializer(init_val);
    return global_variable;
}
