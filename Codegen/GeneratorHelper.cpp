#include "GeneratorHelper.h"
#include "Common.h"

#include <sstream>

using namespace llvm;

namespace GeneratorHelper
{
    AllocaInst* create_alloca_at_top(Function* func, const std::string& variable_name, Type* type) 
    {
        static IRBuilder<> entry_builder(&func->getEntryBlock(), func->getEntryBlock().getFirstInsertionPt());

        entry_builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().getFirstInsertionPt());
        return entry_builder.CreateAlloca(type, nullptr, variable_name);
    }

    binary_operation_fn get_binary_operation_fn(Type* type, const std::string& operation)
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

        static const std::map<std::string, binary_operation_fn>& bool_operations
        {
            {"&", binary_operation_lambda(CreateAnd)},
            {"|", binary_operation_lambda(CreateOr)}
        };

        #undef binary_operation_lambda

        static const std::map<Type*, std::map<std::string, binary_operation_fn>>& type_operation
        {
            {Type::getInt1Ty(get_context()), bool_operations},
            {Type::getInt8Ty(get_context()), integer_operations},
            {Type::getInt16Ty(get_context()), integer_operations},
            {Type::getInt32Ty(get_context()), integer_operations},
            {Type::getInt64Ty(get_context()), integer_operations},
            {Type::getInt128Ty(get_context()), integer_operations},
            {Type::getFloatTy(get_context()), float_operations},
            {Type::getDoubleTy(get_context()), float_operations}
        };

        if (auto operations = type_operation.find(type); operations != type_operation.end())
        {
            if (auto builder_fn = operations->second.find(operation); builder_fn != operations->second.end())
                return builder_fn->second;
        }

        error("This binary operator cannot be applied to the supplied values: " + operation);
    }

    Type* get_type_from_string(const std::string& type)
    {
        static const std::map<std::string, Type*>& type_names
        {
            {"void", Type::getVoidTy(get_context())},
            {"bool", Type::getInt1Ty(get_context())},
            {"i8", Type::getInt8Ty(get_context())},
            {"i16", Type::getInt16Ty(get_context())},
            {"i32", Type::getInt32Ty(get_context())},
            {"i64", Type::getInt64Ty(get_context())},
            {"i128", Type::getInt128Ty(get_context())},
            {"f32", Type::getFloatTy(get_context())},
            {"f64", Type::getDoubleTy(get_context())},
            {"ptr", PointerType::get(get_context(), 0)}
        };

        if (auto type_ = type_names.find(type); type_ != type_names.end())
            return type_->second;

        if (auto struct_type = StructType::getTypeByName(get_context(), type))
            return struct_type;

        error("Invalid type: " + type);
    }
}