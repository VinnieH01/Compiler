#include "GeneratorHelper.h"
#include "../Common.h"

#include <sstream>
#include <unordered_set>

using namespace llvm;
using namespace nlohmann;

namespace GeneratorHelper
{
    ConstantInt* const ZeroConstants::int_32 = ConstantInt::get(get_context(), APInt(32, 0, true));
    ConstantFP* const ZeroConstants::float_32 = ConstantFP::get(get_context(), APFloat(0.0));

    AllocaInst* create_alloca_at_top(Function* func, const std::string& variable_name, Type* type) 
    {
        static IRBuilder<> entry_builder(&func->getEntryBlock(), func->getEntryBlock().getFirstInsertionPt());

        entry_builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().getFirstInsertionPt());
        return entry_builder.CreateAlloca(type, nullptr, variable_name);
    }

    Result<binary_operation_fn, std::shared_ptr<LangError>> get_binary_operation_fn(Type* type, const std::string& operation)
    {
        #define binary_operation_lambda(operation) [](IRBuilder<>& builder, Value* lhs, Value* rhs) { return builder.operation(lhs, rhs); }

        static const std::unordered_map<std::string, binary_operation_fn>& integer_operations
        {
            {Operators::plus, binary_operation_lambda(CreateAdd)},
            {Operators::minus, binary_operation_lambda(CreateSub)},
            {Operators::times, binary_operation_lambda(CreateMul)},
            {Operators::less_than, binary_operation_lambda(CreateICmpSLT)},
            {Operators::greater_than, binary_operation_lambda(CreateICmpSGT)},
            {Operators::equals, binary_operation_lambda(CreateICmpEQ)}
        };

        static const std::unordered_map<std::string, binary_operation_fn>& float_operations
        {    
            {Operators::plus, binary_operation_lambda(CreateFAdd)},
            {Operators::minus, binary_operation_lambda(CreateFSub)},
            {Operators::times, binary_operation_lambda(CreateFMul)},
            {Operators::less_than, binary_operation_lambda(CreateFCmpULT)},
            {Operators::greater_than, binary_operation_lambda(CreateFCmpUGT)},
            {Operators::equals, binary_operation_lambda(CreateFCmpUEQ)}
        };

        static const std::unordered_map<std::string, binary_operation_fn>& bool_operations
        {
            {Operators::boolean_and, binary_operation_lambda(CreateAnd)},
            {Operators::boolean_or, binary_operation_lambda(CreateOr)}
        };

        #undef binary_operation_lambda

        static const std::unordered_map<Type*, std::unordered_map<std::string, binary_operation_fn>>& type_operation
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

        return std::make_shared<LangError>("Binary operator '" + operation + "' cannot be applied to the supplied values");
    }

    Result<Type*, std::shared_ptr<LangError>> get_type_from_string(const std::string& type)
    {
        static const std::unordered_map<std::string, Type*>& type_names
        {
            {TypeNames::void_type, Type::getVoidTy(get_context())},
            {TypeNames::boolean, Type::getInt1Ty(get_context())},
            {TypeNames::int_8, Type::getInt8Ty(get_context())},
            {TypeNames::int_16, Type::getInt16Ty(get_context())},
            {TypeNames::int_32, Type::getInt32Ty(get_context())},
            {TypeNames::int_64, Type::getInt64Ty(get_context())},
            {TypeNames::int_128, Type::getInt128Ty(get_context())},
            {TypeNames::float_32, Type::getFloatTy(get_context())},
            {TypeNames::float_64, Type::getDoubleTy(get_context())},
            {TypeNames::pointer, PointerType::get(get_context(), 0)}
        };

        if (auto type_ = type_names.find(type); type_ != type_names.end())
            return type_->second;

        if (auto struct_type = StructType::getTypeByName(get_context(), type))
            return struct_type;

        return std::static_pointer_cast<LangError>(std::make_shared<InvalidSymbolError>(type));
    }

    bool is_control_flow_terminator(const ASTNode* const node)
    {
        static const std::unordered_set<std::string> termination_nodes
        {
            "Return",
            "LoopTermination"
        };

        return termination_nodes.count(node->get_type()) > 0;
    }

    Type* get_allocated_type(Value* variable_ptr) 
    {
        return isa<GlobalVariable>(variable_ptr)
            ? cast<GlobalVariable>(variable_ptr)->getValueType()
            : cast<AllocaInst>(variable_ptr)->getAllocatedType();
    }
}