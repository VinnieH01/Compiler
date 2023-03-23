#pragma once
#include <memory>
#include <map>
#include <stack>
#include <tuple>
#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRBuilder.h>

#include "third-party/json.hpp"
#include "Struct.h"
#include "ScopeManager.h"
#include "FunctionSignatureManager.h"

class IRGenerator
{
private:
	const std::unique_ptr<llvm::Module>& module;
	const std::unique_ptr<llvm::IRBuilder<>>& builder;
	const std::unique_ptr<llvm::legacy::FunctionPassManager>& function_pass_manager;

	//std::map<std::string, llvm::AllocaInst*> named_values;
    //std::stack<std::map<std::string, llvm::AllocaInst*>> named_values;
    std::map<std::string, Struct> named_types;
    ScopeManager scope_manager;
    FunctionSignatureManager fsm;

    //This is used to keep track of where a "break" or "continue" keyword should branch to.
    std::stack<std::tuple<llvm::BasicBlock*, llvm::BasicBlock*>> loop_stack;

    llvm::Value* visit_literal_node(const nlohmann::json& node);
    llvm::Value* visit_variable_node(const nlohmann::json& node);
    llvm::Value* visit_let_node(const nlohmann::json& node);
    llvm::Value* visit_binary_node(const nlohmann::json& node);
    llvm::Value* visit_unary_node(const nlohmann::json& node);
    llvm::Value* visit_return_node(const nlohmann::json& node);
    llvm::Function* visit_prototype_node(const nlohmann::json& node);
    llvm::Value* visit_function_node(const nlohmann::json& node);
    llvm::Value* visit_call_node(const nlohmann::json& node);
    llvm::Value* visit_if_node(const nlohmann::json& node);
    llvm::Value* visit_loop_node(const nlohmann::json& node);
    llvm::Value* visit_loop_termination_node(const nlohmann::json& node);
    llvm::Value* visit_cast_node(const nlohmann::json& node);
    llvm::Value* visit_dereference_node(const nlohmann::json& node);
    llvm::Value* visit_struct_instance_node(const nlohmann::json& node);
    llvm::Value* visit_struct_definition_node(const nlohmann::json& node);
public:
    IRGenerator(const std::unique_ptr<llvm::Module>&, const std::unique_ptr<llvm::IRBuilder<>>&, const std::unique_ptr<llvm::legacy::FunctionPassManager>&);
    llvm::Value* visit_node(const nlohmann::json& data);
};

