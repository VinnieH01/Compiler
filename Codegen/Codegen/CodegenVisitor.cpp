#include <llvm/IR/Verifier.h>
#include <functional>
#include <stdexcept>
#include <assert.h>

#include "GeneratorHelper.h"
#include "../Common.h"
#include "CodegenVisitor.h"
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#define visit_ret(val) return

using namespace nlohmann;
using namespace llvm;
using namespace GeneratorHelper;

CodegenVisitor::CodegenVisitor()
    :
    module("Module", get_context()),
    builder(get_context()),
    fpm(&module),
    scope_manager(module),
    struct_manager()
{
    fpm.add(createPromoteMemoryToRegisterPass());
    fpm.add(createInstructionCombiningPass());
    fpm.add(createReassociatePass());
    fpm.add(createGVNPass());
    fpm.add(createCFGSimplificationPass());
    fpm.add(createDeadCodeEliminationPass());

    fpm.doInitialization();


    //We never want to mangle the main function
    fsm.add_unmangled_name("main", {});
}

ValueResult CodegenVisitor::visit_literal_node(const LiteralNode& literal)
{
    static std::unordered_map<std::string, std::function<Value* (const LiteralNode&)>> type_to_const
    {
        #define lit_lambda [this](const LiteralNode& lit)

        {TypeNames::boolean,        lit_lambda { return ConstantInt::get(get_context(), APInt(1, lit.get_value<uint64_t>())); } },
        {TypeNames::int_8,          lit_lambda { return ConstantInt::get(get_context(), APInt(8, lit.get_value<uint64_t>())); }},
        {TypeNames::int_16,         lit_lambda { return ConstantInt::get(get_context(), APInt(16, lit.get_value<uint64_t>())); }},
        {TypeNames::int_32,         lit_lambda { return ConstantInt::get(get_context(), APInt(32, lit.get_value<uint64_t>())); }},
        {TypeNames::default_int,    lit_lambda { return ConstantInt::get(get_context(), APInt(32, lit.get_value<uint64_t>())); }}, //Default for int is 32 bit
        {TypeNames::int_64,         lit_lambda { return ConstantInt::get(get_context(), APInt(64, lit.get_value<uint64_t>())); }},
        {TypeNames::int_128,        lit_lambda { return ConstantInt::get(get_context(), APInt(128, lit.get_value<uint64_t>())); }},
        {TypeNames::float_32,       lit_lambda { return ConstantFP::get(Type::getFloatTy(get_context()), lit.get_value<double>()); }},
        {TypeNames::default_float,  lit_lambda { return ConstantFP::get(Type::getFloatTy(get_context()), lit.get_value<double>()); }}, //Default for float is 32 bit
        {TypeNames::float_64,       lit_lambda { return ConstantFP::get(Type::getDoubleTy(get_context()), lit.get_value<double>()); }},
        {TypeNames::pointer,        lit_lambda 
        {
            return builder.CreateIntToPtr(ConstantInt::get(get_context(), APInt(64, lit.get_value<uint64_t>())), PointerType::get(get_context(), 0));
        }},
        {TypeNames::string,         lit_lambda 
        {
            //TODO: This will create duplicates which we dont want
            return builder.CreateGlobalStringPtr(lit.get_value<std::string>(), "", 0, &module);
        }}

        #undef lit_lambda
    };

    if (auto func = type_to_const.find(literal.get_data_type()); func != type_to_const.end())
        return func->second(literal);

    return std::make_shared<LangError>("Cannot get datatype of literal");
}

ValueResult CodegenVisitor::visit_variable_node(const VariableNode& variable)
{
    auto allocation_res = scope_manager.get_variable_allocation(variable.get_name());
    if (allocation_res.is_error()) return allocation_res.get_error();

    Value* allocation = *allocation_res;
    Type* type = get_allocated_type(allocation);

    return builder.CreateLoad(type, allocation);
}

ValueResult CodegenVisitor::visit_let_node(const LetNode& let)
{
    auto value_res = let.get_value()->accept(*this);
    if (value_res.is_error()) return value_res.get_error();

    Value* value = *value_res;

    auto type_res = get_type_from_string(let.get_data_type());
    if (type_res.is_error()) return type_res.get_error();

    Type* type = *type_res;
    std::string variable_name = let.get_name();

    if (type != value->getType())
        return std::make_shared<LangError>("Incompatible types in let assignment: " + variable_name);

    if (scope_manager.is_global_scope())
    {
        return scope_manager.create_global_variable(variable_name, type, value);
    }

    Function* function = builder.GetInsertBlock()->getParent();

    AllocaInst* alloca_inst = create_alloca_at_top(function, variable_name, type);
    auto res = scope_manager.add_local_variable(variable_name, alloca_inst);
    if (res.is_error()) return res.get_error();

    return builder.CreateStore(value, alloca_inst);
}

ValueResult CodegenVisitor::visit_binary_node(const BinaryNode& binary)
{
    const std::string& operation = binary.get_operator();
    const auto& lhs_node = binary.get_left();
    const auto& rhs_node = binary.get_right();

    //This is a special case as we don't want to evaluate the lhs
    if (operation == Operators::assignment)
    {
        std::string variable_name = static_cast<VariableNode*>(lhs_node.get())->get_name();

        auto lhs_res = scope_manager.get_variable_allocation(variable_name);
        if (lhs_res.is_error()) return lhs_res.get_error();

        auto rhs_res = rhs_node->accept(*this);
        if (rhs_res.is_error()) return rhs_res.get_error();

        Value* lhs_allocation = *lhs_res;
        Value* rhs_value = *rhs_res;

        Type* type = get_allocated_type(lhs_allocation);

        if (rhs_value->getType() != type)
            return std::make_shared<LangError>("Incompatible types in assignment: " + variable_name);

        return builder.CreateStore(rhs_value, lhs_allocation);
    }
    
    if (operation == Operators::dot)
    {
        auto struct_allocation_res = scope_manager.get_variable_allocation(static_cast<VariableNode*>(lhs_node.get())->get_name());
        if (struct_allocation_res.is_error()) return struct_allocation_res.get_error();

        Value* struct_ptr = *struct_allocation_res;
        Type* struct_type = get_allocated_type(struct_ptr);

        if (!struct_type->isStructTy())
            return std::make_shared<LangError>("Cannot use indexing on non struct");

        const std::string& field_name = static_cast<VariableNode*>(rhs_node.get())->get_name();

        auto index_res = struct_manager.get_field_index(cast<StructType>(struct_type), field_name);
        if (index_res.is_error()) return index_res.get_error();

        return builder.CreateStructGEP(struct_type, struct_ptr, *index_res);
    }

    auto lhs_res = lhs_node->accept(*this);
    if (lhs_res.is_error()) return lhs_res.get_error();

    auto rhs_res = rhs_node->accept(*this);
    if (rhs_res.is_error()) return rhs_res.get_error();

    Value* lhs_value = *lhs_res;
    Value* rhs_value = *rhs_res;

    Type* lhs_type = lhs_value->getType();

    if (operation == Operators::pointee_assignment)
    {
        if (!lhs_type->isPointerTy())
            return std::make_shared<LangError>("Cannot use poinee assignment on non pointer " + static_cast<VariableNode*>(lhs_node.get())->get_name());
        return builder.CreateStore(rhs_value, lhs_value);
    }

    if (lhs_type != rhs_value->getType())
        return std::make_shared<LangError>("Incompatible types in binary operator");

    auto binary_operation_res = get_binary_operation_fn(lhs_type, operation);
    if (binary_operation_res.is_error()) return binary_operation_res.get_error();
    return (*binary_operation_res)(builder, lhs_value, rhs_value);
}

ValueResult CodegenVisitor::visit_unary_node(const UnaryNode& unary)
{
    std::string operation = unary.get_operator();
    const auto& operand_node = unary.get_operand();

    //Address of operation is a special cases
    if (operation == Operators::adress_of)
    {
        if (operand_node->get_type() != "Variable")
            return std::make_shared<LangError>("Cannot get adress of " + operand_node->get_type());
        return scope_manager.get_variable_allocation(static_cast<VariableNode*>(operand_node.get())->get_name());
    }

    auto operand_res = operand_node->accept(*this);
    if (operand_res.is_error()) return operand_res.get_error();

    Value* operand = *operand_res;
    Type* type = operand->getType();

    //TODO: For now only i32 and f64 support unary operators.
    static std::unordered_map<Type*, std::unordered_map<std::string, std::function<Value* (Value*)>>> type_operation
    {
        {Type::getInt32Ty(get_context()),
        {
            {Operators::plus, [this](Value* o) { return builder.CreateAdd(ZeroConstants::int_32, o); }},
            {Operators::minus, [this](Value* o) { return builder.CreateSub(ZeroConstants::int_32, o); }},
        }},
        {Type::getDoubleTy(get_context()),
        {
            {Operators::plus, [this](Value* o) { return builder.CreateFAdd(ZeroConstants::float_32, o); }},
            {Operators::minus, [this](Value* o) { return builder.CreateFSub(ZeroConstants::float_32, o); }},
        }},
        {Type::getInt1Ty(get_context()),
        {
            {Operators::boolean_not, [this](Value* o) { return builder.CreateNot(o); }},
        }}
    };

    if (auto operations = type_operation.find(type); operations != type_operation.end())
    {
        if (auto builder_fn = operations->second.find(operation); builder_fn != operations->second.end())
            return builder_fn->second(operand);
    }

    return std::make_shared<LangError>("This unary operator cannot be applied to the supplied value: " + operation);
}

ValueResult CodegenVisitor::visit_return_node(const ReturnNode& return_node)
{
    auto val_res = return_node.get_value()->accept(*this);
    if (val_res.is_error()) return val_res.get_error();

    Value* ret_val = *val_res;
    return builder.CreateRet(ret_val);
}

ValueResult CodegenVisitor::visit_prototype_node(const PrototypeNode& prototype)
{
    std::vector<std::string> args = prototype.get_args();
    std::string name = prototype.get_name();

    std::vector<Type*> arg_types;
    for (const auto& type_str : prototype.get_arg_types())
    {
        auto type_res = get_type_from_string(type_str);
        if (type_res.is_error()) return type_res.get_error();

        arg_types.push_back(*type_res);
    }

    auto return_type_res = get_type_from_string(prototype.get_ret_type());
    if (return_type_res.is_error()) return return_type_res.get_error();

    Type* return_type = *return_type_res;
    FunctionType* func_type = FunctionType::get(return_type, arg_types, false);

    //If it is declared as extern we don't want any name mangling
    if (prototype.get_is_extern())
        fsm.add_unmangled_name(name, arg_types);

    Function* function = Function::Create(func_type, Function::ExternalLinkage, fsm.get_function_name(name, arg_types), module);

    // Set names for all arguments.
    size_t idx = 0;
    for (Argument& arg : function->args()) 
        arg.setName(args[idx++]);

    return function;
}

ValueResult CodegenVisitor::visit_function_node(const FunctionNode& function_node)
{
    const PrototypeNode* const prototype = function_node.get_prototype();

    //If function is already declared we dont want to redeclare
    Function* function = module.getFunction(prototype->get_name());
    if (!function)
    {
        auto prototype_res = prototype->accept(*this);
        if (prototype_res.is_error()) return prototype_res.get_error();

        function = cast<Function>(*prototype_res);
    }

    BasicBlock* function_block = BasicBlock::Create(get_context(), "entry", function);
    builder.SetInsertPoint(function_block);

    scope_manager.push_scope();
    for (Argument& arg : function->args())
    {
        //Create an alloca for this variable at the start of the function
        AllocaInst* alloca_inst = create_alloca_at_top(function, arg.getName().str(), arg.getType());

        //Store the initial value in the alloca
        builder.CreateStore(&arg, alloca_inst);

        scope_manager.add_local_variable(std::string(arg.getName()), alloca_inst);
    }

    auto errors = std::make_shared<LangErrorList>();
    for (const auto& node : function_node.get_body())
    {
        auto node_res = node->accept(*this);
        if (node_res.is_error()) errors->add_error(node_res.get_error());

        if (node->get_type() == "Return")
        {
            //If we return there is no point in generating further instructions
            break;
        }
    }

    if (errors->has_errors()) return std::static_pointer_cast<LangError>(errors);

    //The user doesn't need to add returns at the end of void functions so we have to do it here
    if (function->getReturnType()->isVoidTy())
    {
        builder.CreateRetVoid();
    }

    //We are leaving the function scope and want to remove it
    scope_manager.pop_scope();

    //If we leave a function but are still in a non global scope something is wrong
    assert(scope_manager.is_global_scope());

    //verifyFunction returns true if it fails
    if (verifyFunction(*function, &outs()))
        return std::make_shared<LangError>("Function verification failed: " + function->getName().str());

    //Optimize function
    fpm.run(*function);

    return function;
}

ValueResult CodegenVisitor::visit_call_node(const CallNode& call)
{
    auto& arguments = call.get_args();

    std::vector<Value*> arg_values;
    std::vector<Type*> arg_types;
    for (const auto& arg : arguments)
    {
        auto arg_res = arg->accept(*this);
        if (arg_res.is_error()) return arg_res.get_error();

        Value* val = *arg_res;
        arg_values.push_back(val);
        arg_types.push_back(val->getType());
    }

    const auto& callee_name = fsm.get_function_name(call.get_callee(), arg_types);
    Function* callee = module.getFunction(callee_name);
    if (!callee)
        return std::make_shared<LangError>("Function does not exist: " + call.get_callee());

    return builder.CreateCall(callee, arg_values);
}

ValueResult CodegenVisitor::visit_if_node(const IfNode& if_statement)
{
    auto condition_res = if_statement.get_condition()->accept(*this);
    if (condition_res.is_error()) return condition_res.get_error();

    Value* condition = *condition_res;

    if (!condition->getType()->isIntegerTy(1))
        return std::make_shared<LangError>("If statement requires bool type");

    Function* function = builder.GetInsertBlock()->getParent();

    // Create blocks for the then and else cases, as well as continuation which is what happens after both if blocks.
    BasicBlock* then_block = BasicBlock::Create(get_context(), "then");
    BasicBlock* else_block = BasicBlock::Create(get_context(), "else");
    BasicBlock* continuation_block = BasicBlock::Create(get_context(), "ifcont");

    //Branch to then if condition otherwise to else
    builder.CreateCondBr(condition, then_block, else_block);

    auto errors = std::make_shared<LangErrorList>();
    auto generate_block = [&](BasicBlock* block, const std::vector<std::unique_ptr<ASTNode>>& block_nodes)
    {
        block->insertInto(function);
        builder.SetInsertPoint(block);

        scope_manager.push_scope();

        bool found_terminator = false;
        for (const auto& body_node : block_nodes)
        {
            //If we find a return or break we want to stop generating this block since nothing can run after the return/break anyway.
            //Without this the verifier would complain and the function would be invalid because there can only be one termination point.
            auto node_res = body_node->accept(*this);
            if (node_res.is_error()) errors->add_error(node_res.get_error());

            found_terminator = is_control_flow_terminator(body_node.get());
            if (found_terminator) break;
        }

        if (!found_terminator)
        {
            //If we didn't terminate in the above loop we have to do so at the end of the block
            builder.CreateBr(continuation_block);
        }

        scope_manager.pop_scope();
    };

    generate_block(then_block, if_statement.get_then());
    generate_block(else_block, if_statement.get_else());

    if (errors->has_errors()) return std::static_pointer_cast<LangError>(errors);

    //Add the continue block at the end. This is not a new scope, simply a return to the old one
    continuation_block->insertInto(function);
    builder.SetInsertPoint(continuation_block);

    //TODO: What to return here...?
    return then_block;
}

ValueResult CodegenVisitor::visit_loop_node(const LoopNode& loop)
{
    Function* function = builder.GetInsertBlock()->getParent();

    BasicBlock* loop_block = BasicBlock::Create(get_context(), "loop");
    BasicBlock* continuation_block = BasicBlock::Create(get_context(), "loopcont");

    //Save these blocks so the "break" and "continue keywords know where to branch to.
    loop_stack.push({ loop_block, continuation_block });

    //We need to branch here because LLVM requires all blocks be terminated somehow. The flow cannot "fall through" into the loop block.
    builder.CreateBr(loop_block);

    loop_block->insertInto(function);
    builder.SetInsertPoint(loop_block);

    scope_manager.push_scope();
    bool found_terminator = false;
    auto errors = std::make_shared<LangErrorList>();
    for (const auto& body_node : loop.get_body())
    {
        //If we find a "return" or "break/continue" we want to break out of this loop since nothing can run after the return/break anyway.
        //Without this the verifier would complain and the function would be invalid.
        auto node_res = body_node->accept(*this);
        if (node_res.is_error()) errors->add_error(node_res.get_error());

        found_terminator = is_control_flow_terminator(body_node.get());
        if (found_terminator) break;
    }

    //We can only have one terminator
    //Normally this bool would not be set since the code "loop {...; break; ...;}" is unnecessary use of a loop... but it is valid so we need to check for it.
    if (!found_terminator)
    {
        //Branch back to the top
        builder.CreateBr(loop_block);
    }

    if (errors->has_errors()) return std::static_pointer_cast<LangError>(errors);

    scope_manager.pop_scope();

    //We're done with the loop's body so we should remove it from the stack so that the outer loop 
    //is on top of the stack (if there is an outer one)
    loop_stack.pop();

    //Add the continue block at the end
    continuation_block->insertInto(function);
    builder.SetInsertPoint(continuation_block);

    //TODO: What to return here...?
    return loop_block;
}

ValueResult CodegenVisitor::visit_loop_termination_node(const LoopTerminationNode& termination)
{
    if (loop_stack.size() == 0)
        return std::make_shared<LangError>("Cannot break/continue outside loop");

    //Note: "continue_block" refers to the continue keyword and not the "continuation" block
    auto& [continue_block, break_block] = loop_stack.top();

    BasicBlock* branch_to = termination.get_break() ? break_block : continue_block;

    return builder.CreateBr(branch_to);
}

ValueResult CodegenVisitor::visit_cast_node(const CastNode& cast)
{
    auto cast_res = cast.get_value()->accept(*this);
    if (cast_res.is_error()) return cast_res.get_error();

    Value* before_cast = *cast_res;
    Type* type_before = before_cast->getType();

    auto type_after_res = get_type_from_string(cast.get_data_type());
    if (type_after_res.is_error()) return type_after_res.get_error();

    Type* type_after = *type_after_res;

    //If the types are the same we don't need to perform a cast
    if (type_before == type_after) return before_cast;

    bool bit_extension = type_after->getPrimitiveSizeInBits() > type_before->getPrimitiveSizeInBits();

    if (type_before->isIntegerTy() && type_after->isIntegerTy())
        //If the new type is larger we want to performe a signed extension of the bits otherwise we truncate
        return bit_extension ? builder.CreateSExt(before_cast, type_after)
        : builder.CreateTrunc(before_cast, type_after);

    if (type_before->isFloatingPointTy() && type_after->isFloatingPointTy())
        return bit_extension ? builder.CreateFPExt(before_cast, type_after)
        : builder.CreateFPTrunc(before_cast, type_after);

    if (type_before->isFloatingPointTy() && type_after->isIntegerTy())
        return builder.CreateFPToSI(before_cast, type_after);

    if (type_before->isIntegerTy() && type_after->isFloatingPointTy())
        return builder.CreateSIToFP(before_cast, type_after);

    if (type_before->isIntegerTy() && type_after->isPointerTy())
        return builder.CreateIntToPtr(before_cast, type_after);

    if (type_before->isPointerTy() && type_after->isIntegerTy())
        return builder.CreatePtrToInt(before_cast, type_after);

    return std::make_shared<LangError>("Cant perform cast");
}

ValueResult CodegenVisitor::visit_dereference_node(const DereferenceNode& deref)
{
    auto var_res = deref.get_variable()->accept(*this);
    if (var_res.is_error()) return var_res.get_error();

    Value* variable = *var_res;
    if (!variable->getType()->isPointerTy())
        return std::make_shared<LangError>("Cannot dereference non pointer: " + static_cast<VariableNode*>(deref.get_variable().get())->get_name());

    auto deref_type_res = get_type_from_string(deref.get_data_type());
    if (deref_type_res.is_error()) return deref_type_res.get_error();

    return builder.CreateLoad(*deref_type_res, variable);
}

ValueResult CodegenVisitor::visit_struct_instance_node(const StructInstanceNode& struct_inst)
{
    if (scope_manager.is_global_scope())
        return std::make_shared<LangError>("Can only create struct in function"); //For now we cant declare global structs. They have to be pointers

    Function* func = builder.GetInsertBlock()->getParent();

    std::vector<Value*> members;
    for (auto& member_node : struct_inst.get_members())
    {
        auto member_res = member_node->accept(*this);
        if (member_res.is_error()) return member_res.get_error();
        members.push_back(*member_res);
    }

    auto struct_type_res = get_type_from_string(struct_inst.get_data_type());
    if (struct_type_res.is_error()) return struct_type_res.get_error();

    AllocaInst* struct_alloc = create_alloca_at_top(func, "struct", *struct_type_res);

    //Then get pointers to each member of the struct and set the value at that pointer to be the value of the member we are initializing it to
    for (unsigned i = 0; i < members.size(); ++i)
    {
        Value* ptr_to_member = builder.CreateStructGEP(*struct_type_res, struct_alloc, i);
        builder.CreateStore(members[i], ptr_to_member);
    }

    //Finally get the struct from the alloca
    return builder.CreateLoad(*struct_type_res, struct_alloc);
}

ValueResult CodegenVisitor::visit_struct_definition_node(const StructDefinitionNode& struct_def)
{
    std::vector<Type*> types;
    for (const auto& type_str : struct_def.get_member_types())
    {
        auto type_res = get_type_from_string(type_str);
        if (type_res.is_error()) return type_res.get_error();

        types.push_back(*type_res);
    }

    auto* type = StructType::create(get_context(), types, struct_def.get_name());
    struct_manager.add_struct(type, struct_def.get_member_names());

    return nullptr;
}
