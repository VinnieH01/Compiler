#pragma once
#include <memory>
#include <map>
#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRBuilder.h>

#include "json.hpp"

class IRGenerator
{
private:
	const std::unique_ptr<llvm::LLVMContext>& context;
	const std::unique_ptr<llvm::Module>& module;
	const std::unique_ptr<llvm::IRBuilder<>>& builder;
	const std::unique_ptr<llvm::legacy::FunctionPassManager>& function_pass_manager;

	std::map<std::string, llvm::AllocaInst*> named_values;

    llvm::Value* visit_number_node(const nlohmann::json& data);
    llvm::Value* visit_variable_node(const nlohmann::json& data);
    llvm::Value* visit_let_node(const nlohmann::json& data);
    llvm::Value* visit_binary_node(const nlohmann::json& data);
    llvm::Value* visit_unary_node(const nlohmann::json& data);
    llvm::Value* visit_return_node(const nlohmann::json& data);
    llvm::Function* visit_prototype_node(const nlohmann::json& data);
    llvm::Value* visit_function_node(const nlohmann::json& data);
    llvm::Value* visit_call_node(const nlohmann::json& data);
    llvm::Value* visit_if_node(const nlohmann::json& data);

    llvm::AllocaInst* create_alloca_at_top(llvm::Function* func, const std::string& variable_name);
    llvm::GlobalVariable* create_global_variable(const std::string& variable_name, llvm::Constant* init_val);

    bool in_function;
public:
    IRGenerator(const std::unique_ptr<llvm::LLVMContext>&, const std::unique_ptr<llvm::Module>&, const std::unique_ptr<llvm::IRBuilder<>>&, const std::unique_ptr<llvm::legacy::FunctionPassManager>&);
    llvm::Value* visit_node(const nlohmann::json& data);
};

