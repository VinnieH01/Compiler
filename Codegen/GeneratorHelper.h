#pragma once

#include <memory>
#include <map>
#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRBuilder.h>

namespace GeneratorHelper
{
    llvm::AllocaInst* create_alloca_at_top(llvm::Function* func, const std::string& variable_name, llvm::Type* type);
    llvm::GlobalVariable* create_global_variable(llvm::Module* module, const std::string& variable_name, llvm::Type* type, llvm::Constant* init_val);

    using binary_operation_fn = std::function<llvm::Value* (llvm::IRBuilder<>*, llvm::Value*, llvm::Value*)>;
    binary_operation_fn get_binary_operation_fn(llvm::LLVMContext* context, llvm::Type* type, const std::string& operation);

    llvm::Value* get_variable(llvm::Module* module, std::map<std::string, llvm::AllocaInst*>& local_variables, const std::string& variable_name); 
}
