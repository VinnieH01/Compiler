#include "ScopeManager.h"

#include <llvm/IR/Module.h>

using namespace llvm;

ScopeManager::ScopeManager(Module& module)
	: module(module)
{
}

Result<void, std::shared_ptr<LangError>> ScopeManager::add_local_variable(const std::string& name, AllocaInst* allocation)
{
    if (local_variables.top().count(name))
    {
        return std::static_pointer_cast<LangError>(
            std::make_shared<SymbolRedeclarationError>(name)
            );
    }

    local_variables.top()[std::string(name)] = allocation;

    return {};
}

ValueResult ScopeManager::create_global_variable(const std::string& name, llvm::Type* type, llvm::Value* init_val)
{
    if (!isa<Constant>(init_val)) 
    {
        return std::make_shared<LangError>("Can only initialize global variable with a constant");
    }

    //This includes other types of values not just global variables (such as function names).
    if (module.getNamedValue(name))
    {
        return std::static_pointer_cast<LangError>(
            std::make_shared<SymbolRedeclarationError>(name)
            );
    }

    return new GlobalVariable(module, type, false, GlobalValue::ExternalLinkage, cast<Constant>(init_val), name);
}

void ScopeManager::push_scope()
{
    local_variables.emplace();
}

void ScopeManager::pop_scope()
{
    local_variables.pop();
}

bool ScopeManager::is_global_scope()
{
    return local_variables.size() == 0;
}

ValueResult ScopeManager::get_variable_allocation(const std::string& name)
{
    //We need a copy because of the pop() method.
    auto local_variables_cpy = local_variables;
    while (!local_variables_cpy.empty())
    {
        //Local variable takes precedence. The higher it is in the stack the the more recent the scope/block is.
        if (auto local_var = local_variables_cpy.top().find(name); local_var != local_variables_cpy.top().end())
            return local_var->second;
        local_variables_cpy.pop();
    }

    if (auto global_var = module.getNamedGlobal(name))
        return global_var;

    return std::static_pointer_cast<LangError>(
        std::make_shared<InvalidSymbolError>(name)
    );
}
