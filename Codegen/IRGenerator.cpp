#include "IRGenerator.h"
#include <llvm/IR/Verifier.h>
#include <functional>
#include <stdexcept>
#include <assert.h>

#include "GeneratorHelper.h"
#include "Common.h"

using namespace nlohmann;
using namespace llvm;
using namespace GeneratorHelper;

IRGenerator::IRGenerator(const std::unique_ptr<LLVMContext>& ctx, const std::unique_ptr<Module>& mdl, const std::unique_ptr<IRBuilder<>>& bldr, const std::unique_ptr<legacy::FunctionPassManager>& fpm)
    : 
    context(ctx), 
    module(mdl), 
    builder(bldr),
    function_pass_manager(fpm), 
    in_function(false)
{
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
        { "Cast", [this](const json& j) { return visit_cast_node(j); }},
        { "Dereference", [this](const json& j) { return visit_dereference_node(j); }},
        { "Struct", [this](const json& j) { return visit_struct_node(j); }}
    };

    if (auto func = dispatch.find(data["type"]); func != dispatch.end()) 
        return func->second(data);
    
    error("Unknown node type in AST: " + (std::string)data["type"]);
}

Value* IRGenerator::visit_literal_node(const json& data)
{
    static std::map<std::string, std::function<Value* (const json&)>> type_to_const
    {
        {"bool", [this](const json& j) { return ConstantInt::get(*context, APInt(1, (uint64_t)j["value"])); } },
        {"i8", [this](const json& j) { return ConstantInt::get(*context, APInt(8, (uint64_t)j["value"])); }},
        {"i16", [this](const json& j) { return ConstantInt::get(*context, APInt(16, (uint64_t)j["value"])); }},
        {"i32", [this](const json& j) { return ConstantInt::get(*context, APInt(32, (uint64_t)j["value"])); }},
        {"integer", [this](const json& j) { return ConstantInt::get(*context, APInt(32, (uint64_t)j["value"])); }}, //Default for int is 32 bit
        {"i64", [this](const json& j) { return ConstantInt::get(*context, APInt(64, (uint64_t)j["value"])); }},
        {"i128", [this](const json& j) { return ConstantInt::get(*context, APInt(128, (uint64_t)j["value"])); }},
        {"f32", [this](const json& j) { return ConstantFP::get(Type::getFloatTy(*context), (double)j["value"]); }},
        {"f64", [this](const json& j) { return ConstantFP::get(Type::getDoubleTy(*context), (double)j["value"]); }},
        {"ptr", [this](const json& j) { 
            if ((uint64_t)j["value"] != 0) error("Cannot create non null pointer literal");
            return ConstantPointerNull::get(PointerType::get(*context, 0)); 
        }},
        {"string", [this](const json& j) {
            //TODO: This will create duplicates which we dont want
            return builder->CreateGlobalStringPtr((std::string)j["value"], "", 0, module.get());
        }}
    };

    if (auto func = type_to_const.find(data["data_type"]); func != type_to_const.end()) 
        return func->second(data);

    error("Cannot get datatype of literal");
}

Value* IRGenerator::visit_variable_node(const json& data)
{
    Value* variable = get_variable(module.get(), named_values, data["name"]);

    Type* type = isa<GlobalVariable>(variable)
        ? cast<GlobalVariable>(variable)->getValueType()
        : cast<AllocaInst>(variable)->getAllocatedType();

    return builder->CreateLoad(type, variable);
}

Value* IRGenerator::visit_let_node(const json& data)
{
    Value* value = visit_node(data["value"]);
    Type* type = get_type_from_string(context.get(), data["data_type"]);
    std::string variable_name = data["name"];

    if (type != value->getType())
        error("Incompatible types in let assignment: " + variable_name);
    
    //If the builder is outside of a function we want to create a global variable
    if (!in_function)
    {
        if (!isa<Constant>(value)) error("Can only initialize global variable with a constant");
        return create_global_variable(module.get(), data["name"], type, cast<Constant>(value));
    }

    if (named_values.count(variable_name))
        error("Cannot redeclare variable: " + variable_name);

    Function* function = builder->GetInsertBlock()->getParent();

    AllocaInst* alloca_inst = create_alloca_at_top(function, data["name"], type);
    named_values[data["name"]] = alloca_inst;

    return builder->CreateStore(value, alloca_inst);
}

Value* IRGenerator::visit_binary_node(const json& data)
{
    std::string operation = data["operator"];

    //This is a special case as we don't want to evaluate the lhs
    if (operation == "<-")
    {
        std::string variable_name = data["left"]["name"];
        Value* rhs_value = visit_node(data["right"]);
        Value* lhs_variable = get_variable(module.get(), named_values, variable_name);

        if (operation == "<-") 
        {
            Type* lhs_variable_type = (isa<GlobalVariable>(lhs_variable)
                ? cast<GlobalVariable>(lhs_variable)->getValueType()
                : cast<AllocaInst>(lhs_variable)->getAllocatedType());

            if (rhs_value->getType() != lhs_variable_type)
                error("Incompatible types in assignment: " + variable_name);

            return builder->CreateStore(rhs_value, lhs_variable);
        }
    }

    //Index (also don't want to evaluate the lhs)
    if (operation == "[]")
    {
        Value* struct_ptr = get_variable(module.get(), named_values, (std::string)data["left"]["name"]);

        Type* struct_type = (isa<GlobalVariable>(struct_ptr)
            ? cast<GlobalVariable>(struct_ptr)->getValueType()
            : cast<AllocaInst>(struct_ptr)->getAllocatedType());

        Value* rhs_value = visit_node(data["right"]);

        if (!struct_type->isStructTy())
            error("Cannot use indexing on non struct");

        unsigned index = 0;
        if (ConstantInt* index_ci = dyn_cast<ConstantInt>(rhs_value))
        {
            index = index_ci->getZExtValue();
            if (index >= struct_type->getNumContainedTypes())
              error("Index out of bounds in struct");
        }
        else { error("Indexing into a struct requires a constant integer index"); }

        return builder->CreateStructGEP(struct_type, struct_ptr, index);
    }

    Value* lhs = visit_node(data["left"]);
    Value* rhs = visit_node(data["right"]);

    if (operation == ":=")
    {
        if (!lhs->getType()->isPointerTy())
            error("Cannot use poinee assignment on non pointer " + (std::string)data["left"]["name"]);
        return builder->CreateStore(rhs, lhs);
    }

    Type* type = lhs->getType();

    if(type != rhs->getType())
        error("Incompatible types in binary operator");

    auto create_binary_operation = get_binary_operation_fn(context.get(), type, operation);
    return create_binary_operation(builder.get(), lhs, rhs);
}

Value* IRGenerator::visit_unary_node(const json& data)
{
    std::string operation = data["operator"];
    const json& operand_node = data["operand"];

    //Address of operation is a special cases
    if (operation == "&") 
    {
        if (operand_node["type"] != "Variable")
            error("Cannot get adress of " + (std::string)operand_node["type"]);

        return get_variable(module.get(), named_values, operand_node["name"]);
    }

    Value* operand = visit_node(operand_node);
    Type* type = operand->getType();

    //TODO: For now only i32 and f64 support unary operators.
    static std::map<Type*, std::map<std::string, std::function<Value* (Value*)>>> type_operation
    {
        {Type::getInt32Ty(*context),
        {
            {"+", [this](Value* o) { return builder->CreateAdd(ConstantInt::get(*context, APInt(32, 0, true)), o); }},
            {"-", [this](Value* o) { return builder->CreateSub(ConstantInt::get(*context, APInt(32, 0, true)), o); }},
        }},
        {Type::getDoubleTy(*context),
        {
            {"+", [this](Value* o) { return builder->CreateFAdd(ConstantFP::get(*context, APFloat(0.0)), o); }},
            {"-", [this](Value* o) { return builder->CreateFSub(ConstantFP::get(*context, APFloat(0.0)), o); }},
        }},
        {Type::getInt1Ty(*context),
        {
            {"!", [this](Value* o) { return builder->CreateNot(o); }},
        }}
    };

    if (auto operations = type_operation.find(type); operations != type_operation.end())
    {
        if (auto builder_fn = operations->second.find(operation); builder_fn != operations->second.end())
            return builder_fn->second(operand);
    }

    error("This unary operator cannot be applied to the supplied value: " + operation);
}

Value* IRGenerator::visit_return_node(const json& data)
{
    Value* ret_val = visit_node(data["value"]);
    return builder->CreateRet(ret_val);
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
        arg_types.push_back(get_type_from_string(context.get(), str));
    }

    Type* return_type = get_type_from_string(context.get(), data["ret_type"]);
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

    //verifyFunction returns true if it fails
    if (verifyFunction(*function, &outs())) 
        error("Function verification failed: " + function->getName().str());

    //Optimize function
    function_pass_manager->run(*function);

    in_function = false;

    return function;
}

Value* IRGenerator::visit_call_node(const json& data)
{
    std::string callee_name = data["callee"];

    // Look up the name in the global module table.
    Function* callee = module->getFunction(callee_name);

    if (!callee)
        error("Function does not exist: " + callee_name);

    std::vector<json> args_data = data["args"];

    std::vector<Value*> arg_values;
    arg_values.reserve(args_data.size());
    for (size_t i = 0; i < args_data.size(); ++i) 
    {
        arg_values.push_back(visit_node(args_data[i]));
    }

    return builder->CreateCall(callee, arg_values);
}

Value* IRGenerator::visit_if_node(const json& data)
{
    Value* condition = visit_node(data["condition"]);

    if (!condition->getType()->isIntegerTy(1))
        error("If statement requires bool type");

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
            //If we find a return or break we want to stop generating this block since nothing can run after the return/break anyway.
            //Without this the verifier would complain and the function would be invalid because there can only be one termination point.
            visit_node(data);
            if (data["type"] == "Return" || data["type"] == "LoopTermination")
                return;
        }

        //If we didn't terminate in the above loop we have to do so at the end of the block
        builder->CreateBr(continuation_block);
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

    //Save these blocks so the "break" and "continue keywords know where to branch to.
    loop_stack.push({ loop_block, continuation_block });

    //We need to branch here because LLVM requires all blocks be terminated somehow. The flow cannot "fall through" into the loop block.
    builder->CreateBr(loop_block);

    loop_block->insertInto(function);
    builder->SetInsertPoint(loop_block);

    json body_data = data["body"];
    bool terminate_in_block = false;

    for (const json& data : body_data)
    {
        //If we find a "return" or "break/continue" we want to break out of this loop since nothing can run after the return/break anyway.
        //Without this the verifier would complain and the function would be invalid.
        visit_node(data);
        if (data["type"] == "Return" || data["type"] == "LoopTermination")
        {
            terminate_in_block = true;
            break;
        }
    }

    //We're done with the loop's body so we should remove it from the stack so that the outer loop 
    //is on top of the stack (if there is an outer one)
    loop_stack.pop();

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
    if(loop_stack.size() == 0)
        error("Cannot break/continue outside loop");

    //Note: "continue_block" refers to the continue keyword and not the "continuation" block
    auto& [continue_block, break_block] = loop_stack.top();

    BasicBlock* branch_to = data["break"] ? break_block : continue_block;

    return builder->CreateBr(branch_to);
}

Value* IRGenerator::visit_cast_node(const json& data)
{
    Value* before_cast = visit_node(data["value"]);
    Type* type_before = before_cast->getType();
    Type* type_after = get_type_from_string(context.get(), data["data_type"]);

    //If the types are the same we don't need to perform a cast
    if (type_before == type_after) return before_cast;

    bool bit_extension = type_after->getPrimitiveSizeInBits() > type_before->getPrimitiveSizeInBits();

    if (type_before->isIntegerTy() && type_after->isIntegerTy())
        //If the new type is larger we want to performe a signed extension of the bits otherwise we truncate
        return bit_extension ? builder->CreateSExt(before_cast, type_after)
            : builder->CreateTrunc(before_cast, type_after);

    if (type_before->isFloatingPointTy() && type_after->isFloatingPointTy()) 
        return bit_extension ? builder->CreateFPExt(before_cast, type_after)
            : builder->CreateFPTrunc(before_cast, type_after);

    if (type_before->isFloatingPointTy() && type_after->isIntegerTy())
        return builder->CreateFPToSI(before_cast, type_after);

    if (type_before->isIntegerTy() && type_after->isFloatingPointTy())
        return builder->CreateSIToFP(before_cast, type_after);
    
    error("Cant perform cast");
}

Value* IRGenerator::visit_dereference_node(const json& data)
{
    Value* variable = visit_node(data["variable"]);
    if (!variable->getType()->isPointerTy())
        error("Cannot dereference non pointer: " + (std::string)data["variable"]["name"]);

    return builder->CreateLoad(get_type_from_string(context.get(), data["data_type"]), variable);
}

Value* IRGenerator::visit_struct_node(const json& data)
{
    if (!in_function)
        error("Can only create struct in function"); //For now we cant declare global structs. They have to be pointers
    
    Function* func = builder->GetInsertBlock()->getParent();

    const json& member_nodes = data["members"];
    std::vector<Value*> members;
    std::vector<Type*> member_types;

    for (const json& node : member_nodes) 
    {
        Value* member = visit_node(node);
        members.push_back(member);
        member_types.push_back(member->getType());
    }

    //First generate the struct type to be {member1.type, member2.type ...} and alloca one such struct
    auto* struct_type = StructType::get(*context, member_types);
    AllocaInst* struct_alloc = create_alloca_at_top(func, "struct", struct_type);
    
    //Then get pointers to each member of the struct and set the value at that pointer to be the value of the member we are initializing it to
    for (unsigned i = 0; i < members.size(); ++i) 
    {
        Value* ptr_to_member = builder->CreateStructGEP(struct_type, struct_alloc, i);
        builder->CreateStore(members[i], ptr_to_member);
    }

    //Finally get the struct from the alloca
    return builder->CreateLoad(struct_type, struct_alloc);
}
