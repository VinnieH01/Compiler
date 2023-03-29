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
#include "StructManager.h"
#include "FunctionSignatureManager.h"
#include "../AST/ASTNodes.hpp"
#include "../AST/ASTVisitor.h"

class CodegenVisitor : public ASTVisitor<llvm::Value*>
{
public:
    CodegenVisitor();

    llvm::Value* visit_literal_node(const LiteralNode& node) override;
    llvm::Value* visit_variable_node(const VariableNode& node) override;
    llvm::Value* visit_let_node(const LetNode& node) override;
    llvm::Value* visit_binary_node(const BinaryNode& node) override;
    llvm::Value* visit_unary_node(const UnaryNode& node) override;
    llvm::Value* visit_return_node(const ReturnNode& node) override;
    llvm::Value* visit_prototype_node(const PrototypeNode& node) override;
    llvm::Value* visit_function_node(const FunctionNode& node) override;
    llvm::Value* visit_call_node(const CallNode& node) override;
    llvm::Value* visit_if_node(const IfNode& node) override;
    llvm::Value* visit_loop_node(const LoopNode& node) override;
    llvm::Value* visit_loop_termination_node(const LoopTerminationNode& node) override;
    llvm::Value* visit_cast_node(const CastNode& node) override;
    llvm::Value* visit_dereference_node(const DereferenceNode& node) override;
    llvm::Value* visit_struct_instance_node(const StructInstanceNode& node) override;
    llvm::Value* visit_struct_definition_node(const StructDefinitionNode& node) override;

    inline const llvm::Module& get_module() const { return module; }

private:
    //const std::unique_ptr<llvm::Module>& module;
    //const std::unique_ptr<llvm::IRBuilder<>> builder;
    llvm::Module module;
    llvm::IRBuilder<> builder;
    llvm::legacy::FunctionPassManager fpm;

    StructManager struct_manager;
    ScopeManager scope_manager;
    FunctionSignatureManager fsm;

    //This is used to keep track of where a "break" or "continue" keyword should branch to.
    std::stack<std::tuple<llvm::BasicBlock*, llvm::BasicBlock*>> loop_stack;
};
