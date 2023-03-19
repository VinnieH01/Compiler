#include "GeneratorHelper.h"
#include "Common.h"

using namespace llvm;

namespace GeneratorHelper
{
    AllocaInst* create_alloca_at_top(Function* func, const std::string& variable_name, Type* type) 
    {
        static IRBuilder<> entry_builder(&func->getEntryBlock(), func->getEntryBlock().getFirstInsertionPt());

        entry_builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().getFirstInsertionPt());
        return entry_builder.CreateAlloca(type, nullptr, variable_name);
    }

    GlobalVariable* create_global_variable(Module* module, const std::string& variable_name, Type* type, Constant* init_val)
    {
        //This includes other types of values not just global variables (such as function names).
        if (module->getNamedValue(variable_name))
            error("Cannot redefine symbol: " + variable_name);

        return new GlobalVariable(*module, type, false, GlobalValue::ExternalLinkage, init_val, variable_name);
    }

    binary_operation_fn get_binary_operation_fn(LLVMContext* context, Type* type, const std::string& operation)
    {
        #define binary_operation_lambda(operation) [](IRBuilder<>* builder, Value* lhs, Value* rhs) { return builder->operation(lhs, rhs); }

        static const std::map<std::string, binary_operation_fn>& integer_operations
        {
            {"+", binary_operation_lambda(CreateAdd)},
            {"-", binary_operation_lambda(CreateSub)},
            {"*", binary_operation_lambda(CreateMul)},
            {"<", binary_operation_lambda(CreateICmpSLT)},
            {">", binary_operation_lambda(CreateICmpSGT)},
            {"=", binary_operation_lambda(CreateICmpEQ)}
        };

        static const std::map<std::string, binary_operation_fn>& float_operations
        {    
            {"+", binary_operation_lambda(CreateFAdd)},
            {"-", binary_operation_lambda(CreateFSub)},
            {"*", binary_operation_lambda(CreateFMul)},
            {"<", binary_operation_lambda(CreateFCmpULT)},
            {">", binary_operation_lambda(CreateFCmpUGT)},
            {"=", binary_operation_lambda(CreateFCmpUEQ)}
        };

        #undef binary_operation_lambda    

        static const std::map<Type*, std::map<std::string, binary_operation_fn>>& type_operation
        {
            {Type::getInt1Ty(*context), integer_operations},
            {Type::getInt8Ty(*context), integer_operations},
            {Type::getInt16Ty(*context), integer_operations},
            {Type::getInt32Ty(*context), integer_operations},
            {Type::getInt64Ty(*context), integer_operations},
            {Type::getInt128Ty(*context), integer_operations},
            {Type::getFloatTy(*context), float_operations},
            {Type::getDoubleTy(*context), float_operations}
        };

        if (auto operations = type_operation.find(type); operations != type_operation.end())
        {
            if (auto builder_fn = operations->second.find(operation); builder_fn != operations->second.end())
                return builder_fn->second;
        }

        error("This binary operator cannot be applied to the supplied values: " + operation);
    }

    Value* get_variable(llvm::Module* module, std::map<std::string, llvm::AllocaInst*>& local_variables, const std::string& variable_name)
    {
        //Local variable takes precedence
        if (auto local_var = local_variables.find(variable_name); local_var != local_variables.end())
            return local_var->second;

        if (auto global_var = module->getNamedGlobal(variable_name))
            return global_var;

        error("Variable does not exist: " + variable_name);
    }
}