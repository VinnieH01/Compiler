#include <llvm/IR/Verifier.h>
#include <functional>
#include <stdexcept>
#include <assert.h>

#include "GeneratorHelper.h"
#include "Common.h"
#include "CodegenVisitor.h"

using namespace nlohmann;
using namespace llvm;
using namespace GeneratorHelper;

CodegenVisitor::CodegenVisitor(const std::unique_ptr<Module>& mdl, const std::unique_ptr<legacy::FunctionPassManager>& fpm)
    :
    module(mdl),
    builder(std::make_unique<IRBuilder<>>(get_context())),
    function_pass_manager(fpm),
    scope_manager(module.get()),
    struct_fields()
{
    //We never want to mangle the main function
    fsm.add_unmangled_name("main", {});
}

Value* CodegenVisitor::visit_literal_node(const LiteralNode& node)
{
    static std::unordered_map<std::string, std::function<Value* (const LiteralNode&)>> type_to_const
    {
        {"bool", [this](const LiteralNode& literal) { return ConstantInt::get(get_context(), APInt(1, literal.get_value<uint64_t>())); } },
        {"i8", [this](const LiteralNode& literal) { return ConstantInt::get(get_context(), APInt(8, literal.get_value<uint64_t>())); }},
        {"i16", [this](const LiteralNode& literal) { return ConstantInt::get(get_context(), APInt(16, literal.get_value<uint64_t>())); }},
        {"i32", [this](const LiteralNode& literal) { return ConstantInt::get(get_context(), APInt(32, literal.get_value<uint64_t>())); }},
        {"integer", [this](const LiteralNode& literal) { return ConstantInt::get(get_context(), APInt(32, literal.get_value<uint64_t>())); }}, //Default for int is 32 bit
        {"i64", [this](const LiteralNode& literal) { return ConstantInt::get(get_context(), APInt(64, literal.get_value<uint64_t>())); }},
        {"i128", [this](const LiteralNode& literal) { return ConstantInt::get(get_context(), APInt(128, literal.get_value<uint64_t>())); }},
        {"f32", [this](const LiteralNode& literal) { return ConstantFP::get(Type::getFloatTy(get_context()), literal.get_value<double>()); }},
        {"f64", [this](const LiteralNode& literal) { return ConstantFP::get(Type::getDoubleTy(get_context()), literal.get_value<double>()); }},
        {"ptr", [this](const LiteralNode& literal) {
            return builder->CreateIntToPtr(ConstantInt::get(get_context(), APInt(64, literal.get_value<uint64_t>())), PointerType::get(get_context(), 0));
        }},
        {"string", [this](const LiteralNode& literal) {
            //TODO: This will create duplicates which we dont want
            return builder->CreateGlobalStringPtr(literal.get_value<std::string>(), "", 0, module.get());
        }}
    };

    if (auto func = type_to_const.find(node.get_data_type()); func != type_to_const.end())
        return func->second(node);

    error("Cannot get datatype of literal");
}

Value* CodegenVisitor::visit_variable_node(const VariableNode& node)
{
    Value* variable = scope_manager.get_variable(node.get_name());

    Type* type = isa<GlobalVariable>(variable)
        ? cast<GlobalVariable>(variable)->getValueType()
        : cast<AllocaInst>(variable)->getAllocatedType();

    return builder->CreateLoad(type, variable);
}

Value* CodegenVisitor::visit_let_node(const LetNode& node)
{
    Value* value = node.get_value()->accept(*this);
    Type* type = get_type_from_string(node.get_data_type());
    std::string variable_name = node.get_name();

    if (type != value->getType())
        error("Incompatible types in let assignment: " + variable_name);

    if (scope_manager.is_global_scope())
    {
        return scope_manager.create_global_variable(node.get_name(), type, value);
    }

    Function* function = builder->GetInsertBlock()->getParent();

    AllocaInst* alloca_inst = create_alloca_at_top(function, node.get_name(), type);
    scope_manager.add_local_variable(node.get_name(), alloca_inst);

    return builder->CreateStore(value, alloca_inst);
}

Value* CodegenVisitor::visit_binary_node(const BinaryNode& node)
{
    std::string operation = node.get_operator();

    //This is a special case as we don't want to evaluate the lhs
    if (operation == "<-")
    {
        std::string variable_name = static_cast<VariableNode*>(node.get_left().get())->get_name();
        Value* rhs_value = node.get_right()->accept(*this);
        Value* lhs_variable = scope_manager.get_variable(variable_name);

        Type* lhs_variable_type = (isa<GlobalVariable>(lhs_variable)
            ? cast<GlobalVariable>(lhs_variable)->getValueType()
            : cast<AllocaInst>(lhs_variable)->getAllocatedType());

        if (rhs_value->getType() != lhs_variable_type)
            error("Incompatible types in assignment: " + variable_name);

        return builder->CreateStore(rhs_value, lhs_variable);
    }

    //Index (also don't want to evaluate the lhs)
    if (operation == "[]")
    {
        Value* struct_ptr = scope_manager.get_variable(static_cast<VariableNode*>(node.get_left().get())->get_name());

        Type* struct_type = (isa<GlobalVariable>(struct_ptr)
            ? cast<GlobalVariable>(struct_ptr)->getValueType()
            : cast<AllocaInst>(struct_ptr)->getAllocatedType());

        Value* rhs_value = node.get_right()->accept(*this);

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

    if (operation == ".")
    {
        Value* struct_ptr = scope_manager.get_variable(static_cast<VariableNode*>(node.get_left().get())->get_name());

        Type* struct_type = (isa<GlobalVariable>(struct_ptr)
            ? cast<GlobalVariable>(struct_ptr)->getValueType()
            : cast<AllocaInst>(struct_ptr)->getAllocatedType());

        if (!struct_type->isStructTy())
            error("Cannot use indexing on non struct");

        std::string field_name = static_cast<VariableNode*>(node.get_right().get())->get_name();

        if (const auto& field_names = struct_fields.find(cast<StructType>(struct_type)); field_names != struct_fields.end())
        {
            if (auto field_it = std::find(field_names->second.begin(), field_names->second.end(), field_name); field_it != field_names->second.end())
            {
                unsigned index = field_it - field_names->second.begin();
                return builder->CreateStructGEP(struct_type, struct_ptr, index);
            }
            error("Invalid field: " + field_name);
        }
        error("Error in field access");
    }

    Value* lhs = node.get_left()->accept(*this);
    Value* rhs = node.get_right()->accept(*this);

    if (operation == ":=")
    {
        if (!lhs->getType()->isPointerTy())
            error("Cannot use poinee assignment on non pointer " + static_cast<VariableNode*>(node.get_left().get())->get_name());
        return builder->CreateStore(rhs, lhs);
    }

    Type* type = lhs->getType();

    if (type != rhs->getType())
        error("Incompatible types in binary operator");

    auto create_binary_operation = get_binary_operation_fn(type, operation);
    return create_binary_operation(builder.get(), lhs, rhs);
}

Value* CodegenVisitor::visit_unary_node(const UnaryNode& node)
{
    std::string operation = node.get_operator();
    const auto& operand_node = node.get_operand();

    //Address of operation is a special cases
    if (operation == "&")
    {
        if (operand_node->get_type() != "Variable")
            error("Cannot get adress of " + (std::string)operand_node->get_type());
        return scope_manager.get_variable(static_cast<VariableNode*>(operand_node.get())->get_name());
    }

    Value* operand = operand_node->accept(*this);
    Type* type = operand->getType();

    //TODO: For now only i32 and f64 support unary operators.
    static std::unordered_map<Type*, std::unordered_map<std::string, std::function<Value* (Value*)>>> type_operation
    {
        {Type::getInt32Ty(get_context()),
        {
            {"+", [this](Value* o) { return builder->CreateAdd(ConstantInt::get(get_context(), APInt(32, 0, true)), o); }},
            {"-", [this](Value* o) { return builder->CreateSub(ConstantInt::get(get_context(), APInt(32, 0, true)), o); }},
        }},
        {Type::getDoubleTy(get_context()),
        {
            {"+", [this](Value* o) { return builder->CreateFAdd(ConstantFP::get(get_context(), APFloat(0.0)), o); }},
            {"-", [this](Value* o) { return builder->CreateFSub(ConstantFP::get(get_context(), APFloat(0.0)), o); }},
        }},
        {Type::getInt1Ty(get_context()),
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

Value* CodegenVisitor::visit_return_node(const ReturnNode& node)
{
    Value* ret_val = node.get_value()->accept(*this);
    return builder->CreateRet(ret_val);
}

Value* CodegenVisitor::visit_prototype_node(const PrototypeNode& node)
{
    std::vector<std::string> args = node.get_args();
    std::vector<std::string> arg_types_str = node.get_arg_types();
    std::string name = node.get_name();

    std::vector<Type*> arg_types;
    arg_types.reserve(arg_types_str.size());
    for (const std::string& str : arg_types_str)
    {
        arg_types.push_back(get_type_from_string(str));
    }

    Type* return_type = get_type_from_string(node.get_ret_type());
    FunctionType* func_type = FunctionType::get(return_type, arg_types, false);

    if (node.get_is_extern())
        fsm.add_unmangled_name(name, arg_types);

    Function* function = Function::Create(func_type, Function::ExternalLinkage, fsm.get_function_name(name, arg_types), module.get());

    // Set names for all arguments.
    size_t idx = 0;
    for (Argument& arg : function->args()) arg.setName(args[idx++]);

    return function;
}

Value* CodegenVisitor::visit_function_node(const FunctionNode& node)
{
    const PrototypeNode* prototype = node.get_prototype();

    //If function is already declared we dont want to redeclare
    Function* function = module->getFunction(prototype->get_name());

    if (!function)
    {
        function = cast<Function>(prototype->accept(*this));
    }

    BasicBlock* function_block = BasicBlock::Create(get_context(), "entry", function);
    builder->SetInsertPoint(function_block);

    scope_manager.push_scope();
    for (Argument& arg : function->args())
    {
        //Create an alloca for this variable at the start of the function
        AllocaInst* alloca_inst = create_alloca_at_top(function, arg.getName().str(), arg.getType());

        //Store the initial value in the alloca
        builder->CreateStore(&arg, alloca_inst);

        scope_manager.add_local_variable(std::string(arg.getName()), alloca_inst);
    }

    auto& body_data = node.get_body();
    for (const auto& data : body_data)
    {
        data->accept(*this);

        if (data->get_type() == "Return")
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

    //We are leaving the function scope and want to remove it
    scope_manager.pop_scope();

    //If we leave a function but are still in a non global scope something is wrong
    assert(scope_manager.is_global_scope());

    //verifyFunction returns true if it fails
    if (verifyFunction(*function, &outs()))
        error("Function verification failed: " + function->getName().str());

    //Optimize function
    function_pass_manager->run(*function);

    return function;
}

Value* CodegenVisitor::visit_call_node(const CallNode& node)
{
    auto& args_data = node.get_args();

    std::vector<Value*> arg_values;
    std::vector<Type*> arg_types;
    arg_values.reserve(args_data.size());
    for (size_t i = 0; i < args_data.size(); ++i)
    {
        Value* arg = args_data[i]->accept(*this);
        arg_values.push_back(arg);
        arg_types.push_back(arg->getType());
    }

    std::string callee_name = fsm.get_function_name(node.get_callee(), arg_types);

    Function* callee = module->getFunction(callee_name);

    if (!callee)
        error("Function does not exist: " + callee_name);

    return builder->CreateCall(callee, arg_values);
}

Value* CodegenVisitor::visit_if_node(const IfNode& node)
{
    Value* condition = node.get_condition()->accept(*this);

    if (!condition->getType()->isIntegerTy(1))
        error("If statement requires bool type");

    Function* function = builder->GetInsertBlock()->getParent();

    // Create blocks for the then and else cases, as well as continuation which is what happens after both if blocks.
    BasicBlock* then_block = BasicBlock::Create(get_context(), "then");
    BasicBlock* else_block = BasicBlock::Create(get_context(), "else");
    BasicBlock* continuation_block = BasicBlock::Create(get_context(), "ifcont");

    //Branch to then if condition otherwise to else
    builder->CreateCondBr(condition, then_block, else_block);

    auto generate_block = [&](BasicBlock* block, const std::vector<std::unique_ptr<ASTNode>>& block_nodes)
    {
        block->insertInto(function);
        builder->SetInsertPoint(block);

        scope_manager.push_scope();

        bool found_terminator = false;
        for (const auto& body_node : block_nodes)
        {
            //If we find a return or break we want to stop generating this block since nothing can run after the return/break anyway.
            //Without this the verifier would complain and the function would be invalid because there can only be one termination point.
            body_node->accept(*this);
            found_terminator = is_control_flow_terminator(body_node.get());
            if (found_terminator) break;
        }

        if (!found_terminator)
        {
            //If we didn't terminate in the above loop we have to do so at the end of the block
            builder->CreateBr(continuation_block);
        }

        scope_manager.pop_scope();
    };

    generate_block(then_block, node.get_then());
    generate_block(else_block, node.get_else());

    //Add the continue block at the end. This is not a new scope, simply a return to the old one
    continuation_block->insertInto(function);
    builder->SetInsertPoint(continuation_block);

    //TODO: What to return here...?
    return then_block;
}

Value* CodegenVisitor::visit_loop_node(const LoopNode& node)
{
    Function* function = builder->GetInsertBlock()->getParent();

    BasicBlock* loop_block = BasicBlock::Create(get_context(), "loop");
    BasicBlock* continuation_block = BasicBlock::Create(get_context(), "loopcont");

    //Save these blocks so the "break" and "continue keywords know where to branch to.
    loop_stack.push({ loop_block, continuation_block });

    //We need to branch here because LLVM requires all blocks be terminated somehow. The flow cannot "fall through" into the loop block.
    builder->CreateBr(loop_block);

    loop_block->insertInto(function);
    builder->SetInsertPoint(loop_block);

    scope_manager.push_scope();
    bool found_terminator = false;
    for (const auto& body_node : node.get_body())
    {
        //If we find a "return" or "break/continue" we want to break out of this loop since nothing can run after the return/break anyway.
        //Without this the verifier would complain and the function would be invalid.
        body_node->accept(*this);
        found_terminator = is_control_flow_terminator(body_node.get());
        if (found_terminator) break;
    }

    //We can only have one terminator
    //Normally this bool would not be set since the code "loop {...; break; ...;}" is unnecessary use of a loop... but it is valid so we need to check for it.
    if (!found_terminator)
    {
        //Branch back to the top
        builder->CreateBr(loop_block);
    }

    scope_manager.pop_scope();

    //We're done with the loop's body so we should remove it from the stack so that the outer loop 
    //is on top of the stack (if there is an outer one)
    loop_stack.pop();

    //Add the continue block at the end
    continuation_block->insertInto(function);
    builder->SetInsertPoint(continuation_block);

    //TODO: What to return here...?
    return loop_block;
}

Value* CodegenVisitor::visit_loop_termination_node(const LoopTerminationNode& node)
{
    if (loop_stack.size() == 0)
        error("Cannot break/continue outside loop");

    //Note: "continue_block" refers to the continue keyword and not the "continuation" block
    auto& [continue_block, break_block] = loop_stack.top();

    BasicBlock* branch_to = node.get_break() ? break_block : continue_block;

    return builder->CreateBr(branch_to);
}

Value* CodegenVisitor::visit_cast_node(const CastNode& node)
{
    Value* before_cast = node.get_value()->accept(*this);
    Type* type_before = before_cast->getType();
    Type* type_after = get_type_from_string(node.get_data_type());

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

    if (type_before->isIntegerTy() && type_after->isPointerTy())
        return builder->CreateIntToPtr(before_cast, type_after);

    if (type_before->isPointerTy() && type_after->isIntegerTy())
        return builder->CreatePtrToInt(before_cast, type_after);

    error("Cant perform cast");
}

Value* CodegenVisitor::visit_dereference_node(const DereferenceNode& node)
{
    Value* variable = node.get_variable()->accept(*this);
    if (!variable->getType()->isPointerTy())
        error("Cannot dereference non pointer: " + static_cast<VariableNode*>(node.get_variable().get())->get_name());

    return builder->CreateLoad(get_type_from_string(node.get_data_type()), variable);
}

Value* CodegenVisitor::visit_struct_instance_node(const StructInstanceNode& node)
{
    if (scope_manager.is_global_scope())
        error("Can only create struct in function"); //For now we cant declare global structs. They have to be pointers

    Function* func = builder->GetInsertBlock()->getParent();

    std::vector<Value*> members;
    for (const auto& node : node.get_members())
    {
        members.push_back(node->accept(*this));
    }

    auto* struct_type = get_type_from_string(node.get_data_type());
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

Value* CodegenVisitor::visit_struct_definition_node(const StructDefinitionNode& node)
{
    std::vector<Type*> types;
    std::vector<std::string> names;
    for (const std::string& type : node.get_member_types())
    {
        types.push_back(get_type_from_string(type));
    }
    for (const std::string& name : node.get_member_names())
    {
        names.push_back(name);
    }
    auto* type = StructType::create(get_context(), types, node.get_name());
    struct_fields.insert({ type, names });


    return nullptr;
}
