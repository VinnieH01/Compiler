#pragma once
#include <stack>
#include <map>
#include <string>
#include <llvm/IR/Instructions.h>
#include "Error/ValueResult.h"

class ScopeManager
{
public:
	ScopeManager(llvm::Module& module);
	Result<void, LangError> add_local_variable(const std::string& name, llvm::AllocaInst* allocation);
	ValueResult create_global_variable(const std::string& name, llvm::Type* type, llvm::Value* init_val);
	void push_scope();
	void pop_scope();
	bool is_global_scope();
	ValueResult get_variable_allocation(const std::string& name);
private:
	std::stack<std::map<std::string, llvm::AllocaInst*>> local_variables;
	llvm::Module& module;
};

