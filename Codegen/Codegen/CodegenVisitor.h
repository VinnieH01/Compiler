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

#include "Error/ValueResult.h"

class CodegenVisitor : public ASTVisitor<ValueResult>
{
public:
    CodegenVisitor();

    ValueResult visit_literal_node(const LiteralNode& node) override;
    ValueResult visit_variable_node(const VariableNode& node) override;
    ValueResult visit_let_node(const LetNode& node) override;
    ValueResult visit_binary_node(const BinaryNode& node) override;
    ValueResult visit_unary_node(const UnaryNode& node) override;
    ValueResult visit_return_node(const ReturnNode& node) override;
    ValueResult visit_prototype_node(const PrototypeNode& node) override;
    ValueResult visit_function_node(const FunctionNode& node) override;
    ValueResult visit_call_node(const CallNode& node) override;
    ValueResult visit_if_node(const IfNode& node) override;
    ValueResult visit_loop_node(const LoopNode& node) override;
    ValueResult visit_loop_termination_node(const LoopTerminationNode& node) override;
    ValueResult visit_cast_node(const CastNode& node) override;
    ValueResult visit_dereference_node(const DereferenceNode& node) override;
    ValueResult visit_struct_instance_node(const StructInstanceNode& node) override;
    ValueResult visit_struct_definition_node(const StructDefinitionNode& node) override;

    inline const llvm::Module& get_module() const { return module; }

private:
    llvm::Module module;
    llvm::IRBuilder<> builder;
    llvm::legacy::FunctionPassManager fpm;

    StructManager struct_manager;
    ScopeManager scope_manager;
    FunctionSignatureManager fsm;

    //This is used to keep track of where a "break" or "continue" keyword should branch to.
    std::stack<std::tuple<llvm::BasicBlock*, llvm::BasicBlock*>> loop_stack;
};
