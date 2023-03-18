#include "GeneratorHelper.h"
#include "Common.h"

using namespace llvm;

namespace GeneratorHelper
{
    AllocaInst* create_alloca_at_top(Function* func, const std::string& variable_name, Type* type) {
        static IRBuilder<> entry_builder(&func->getEntryBlock(), func->getEntryBlock().begin());

        entry_builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
        return entry_builder.CreateAlloca(type, nullptr, variable_name);
    }

    GlobalVariable* create_global_variable(Module* module, const std::string& variable_name, Type* type, Constant* init_val)
    {
        if (module->getNamedGlobal(variable_name))
            error("Cannot redefine global variable: " + variable_name);

        module->getOrInsertGlobal(variable_name, type);
        GlobalVariable* global_variable = module->getNamedGlobal(variable_name);
        global_variable->setLinkage(GlobalValue::ExternalLinkage);
        global_variable->setInitializer(init_val);
        return global_variable;
    }

    std::function<Value* (IRBuilder<>*, Value*, Value*)> get_binary_operation_fn(LLVMContext* context, Type* type, const std::string& operation)
    {
        static std::map<std::string, std::function<Value* (IRBuilder<>*, Value*, Value*)>> integer_operations
        {
            {"+", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateAdd(l, r); }},
            {"-", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateSub(l, r); }},
            {"*", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateMul(l, r); }},
            {"<", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateICmpSLT(l, r); }},
            {">", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateICmpSGT(l, r); }},
            {"=", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateICmpEQ(l, r); }}
        };

        static std::map<std::string, std::function<Value* (IRBuilder<>*, Value*, Value*)>> float_operations
        {
            {"+", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateFAdd(l, r); }},
            {"-", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateFSub(l, r); }},
            {"*", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateFMul(l, r); }},
            {"<", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateFCmpULT(l, r); }},
            {">", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateFCmpUGT(l, r); }},
            {"=", [](IRBuilder<>* builder, Value* l, Value* r) { return builder->CreateFCmpUEQ(l, r); }}
        };

        static std::map<Type*, std::map<std::string, std::function<Value* (IRBuilder<>*, Value*, Value*)>>> type_operation
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

        auto operations = get_or_null(type_operation, type);
        if (operations)
        {
            auto builder_fn = get_or_null(*operations, operation);
            if (builder_fn)
                return *builder_fn;
        }

        error("This binary operator cannot be applied to the supplied values: " + operation);
    }

    Value* get_variable(llvm::Module* module, std::map<std::string, llvm::AllocaInst*>& local_variables, const std::string& variable_name)
    {
        //Local variable takes precedence
        Value* variable = module->getNamedValue(variable_name);
        auto** local_var = get_or_null(local_variables, variable_name);
        if (local_var) variable = *local_var;

        if(!variable) error("Variable does not exist: " + variable_name);
        return variable;
    }
}