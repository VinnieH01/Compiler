#pragma once

#include <memory>
#include <stack>
#include <tuple>
#include <unordered_map>
#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRBuilder.h>

#include "ScopeManager.h"
#include "FunctionSignatureManager.h"

class CodegenVisitor
{
public:
    llvm::Value* visit_literal_node(const class LiteralNode& node);
    llvm::Value* visit_variable_node(const class VariableNode& node);
    llvm::Value* visit_let_node(const class LetNode& node);
    llvm::Value* visit_binary_node(const class BinaryNode& node);
    llvm::Value* visit_unary_node(const class UnaryNode& node);
    llvm::Value* visit_return_node(const class ReturnNode& node);
    llvm::Value* visit_prototype_node(const class PrototypeNode& node);
    llvm::Value* visit_function_node(const class FunctionNode& node);
    llvm::Value* visit_call_node(const class CallNode& node);
    llvm::Value* visit_if_node(const class IfNode& node);
    llvm::Value* visit_loop_node(const class LoopNode& node);
    llvm::Value* visit_loop_termination_node(const class LoopTerminationNode& node);
    llvm::Value* visit_cast_node(const class CastNode& node);
    llvm::Value* visit_dereference_node(const class DereferenceNode& node);
    llvm::Value* visit_struct_instance_node(const class StructInstanceNode& node);
    llvm::Value* visit_struct_definition_node(const class StructDefinitionNode& node);

    const std::unique_ptr<llvm::Module>& module;
    const std::unique_ptr<llvm::IRBuilder<>> builder;
    const std::unique_ptr<llvm::legacy::FunctionPassManager>& function_pass_manager;

    std::unordered_map<llvm::StructType*, std::vector<std::string>> struct_fields;

    ScopeManager scope_manager;
    FunctionSignatureManager fsm;

    //This is used to keep track of where a "break" or "continue" keyword should branch to.
    std::stack<std::tuple<llvm::BasicBlock*, llvm::BasicBlock*>> loop_stack;

    CodegenVisitor(const std::unique_ptr<llvm::Module>&, const std::unique_ptr<llvm::legacy::FunctionPassManager>&);
};
