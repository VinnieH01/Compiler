#pragma once

#include <memory>
#include <map>
#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRBuilder.h>
#include "Struct.h"
#include <stack>

namespace GeneratorHelper
{
    llvm::AllocaInst* create_alloca_at_top(llvm::Function* func, const std::string& variable_name, llvm::Type* type);
    using binary_operation_fn = std::function<llvm::Value* (llvm::IRBuilder<>*, llvm::Value*, llvm::Value*)>;
    binary_operation_fn get_binary_operation_fn(llvm::Type* type, const std::string& operation);
    llvm::Type* get_type_from_string(const std::map<std::string, Struct>&, const std::string& type);
    llvm::Type* get_type_from_string(const std::string& type);
}
