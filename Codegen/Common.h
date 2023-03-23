#pragma once

#include <string>
#include <map>
#include <llvm/IR/LLVMContext.h>

void error(const std::string& message);
llvm::LLVMContext& get_context();