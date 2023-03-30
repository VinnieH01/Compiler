#pragma once

#include <memory>
#include <unordered_map>
#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRBuilder.h>
#include <stack>

#include "../third-party/json.hpp"
#include "../AST/ASTNodes.hpp"
#include "Error/Result.h"
#include "Error/LangError.hpp"

namespace GeneratorHelper
{
    struct Operators
    {
        static constexpr const char* assignment = "<-";
        static constexpr const char* index = "[]";
        static constexpr const char* dot = ".";
        static constexpr const char* adress_of = "&";
        static constexpr const char* boolean_and = "&";
        static constexpr const char* boolean_or = "|";
        static constexpr const char* boolean_not = "!";
        static constexpr const char* pointee_assignment = ":=";
        static constexpr const char* plus = "+";
        static constexpr const char* minus = "-";
        static constexpr const char* times = "*";
        static constexpr const char* greater_than = ">";
        static constexpr const char* less_than = "<";
        static constexpr const char* equals = "=";
    };

    struct TypeNames
    {
        static constexpr const char* void_type = "void";
        static constexpr const char* boolean = "bool";
        static constexpr const char* default_int = "integer";
        static constexpr const char* int_8 = "i8";
        static constexpr const char* int_16 = "i16";
        static constexpr const char* int_32 = "i32";
        static constexpr const char* int_64 = "i64";
        static constexpr const char* int_128 = "i128";
        static constexpr const char* default_float = "float";
        static constexpr const char* float_32 = "f32";
        static constexpr const char* float_64 = "f64";
        static constexpr const char* pointer = "ptr";
        static constexpr const char* string = "string";
    };

    struct ZeroConstants 
    {
        static llvm::ConstantInt* const int_32;
        static llvm::ConstantFP* const float_32;
    };

    llvm::AllocaInst* create_alloca_at_top(llvm::Function* func, const std::string& variable_name, llvm::Type* type);
    using binary_operation_fn = std::function<llvm::Value* (llvm::IRBuilder<>&, llvm::Value*, llvm::Value*)>;
    Result<binary_operation_fn, std::shared_ptr<LangError>> get_binary_operation_fn(llvm::Type* type, const std::string& operation);
    Result<llvm::Type*, std::shared_ptr<LangError>> get_type_from_string(const std::string& type);
    llvm::Type* get_allocated_type(llvm::Value* variable_ptr);
    bool is_control_flow_terminator(const ASTNode* const node);
}
