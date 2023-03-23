#include "FunctionSignatureManager.h"
#include <map>
#include "Common.h"
#include <llvm/IR/DerivedTypes.h>
#include "FunctionSignatureManager.h"

using namespace llvm;

std::string FunctionSignatureManager::get_mangled_type_str(Type* type)
{
    static const std::map<Type*, std::string>& mangled_types
    {
        {Type::getInt1Ty(get_context()), "b"},
        {Type::getInt8Ty(get_context()), "c"},
        {Type::getInt16Ty(get_context()), "s"},
        {Type::getInt32Ty(get_context()), "i"},
        {Type::getInt64Ty(get_context()), "l"},
        {Type::getInt128Ty(get_context()), "L"},
        {Type::getFloatTy(get_context()), "f"},
        {Type::getDoubleTy(get_context()), "d"},
        {PointerType::get(get_context(), 0), "p"}
    };

    if (auto m_type = mangled_types.find(type); m_type != mangled_types.end())
    {
        return m_type->second;
    }

    if (StructType* struct_type = dyn_cast<StructType>(type))
    {
        std::string m_type = "_";
        for (Type* type : struct_type->elements())
        {
            m_type += get_mangled_type_str(type);
        }
        m_type += "_";

        return m_type;
    }

    error("Cannot mangle type");
}

std::string FunctionSignatureManager::get_function_name(const std::string& name, const std::vector<Type*>& arg_types)
{
    //We start with "__" to differentiate between mangled and unmangled names. For example if we have an unmangled "foo(int x)" it becomes "foo".
    //And then we have a mangled "foo()". In this case we dont want foo() to become "foo" as it would clash with the unmangled one desipite having
    //A different signature. Therefore we add __ so it becomes "__foo"
    std::string mangled_name = "__" + name;

    for (Type* type : arg_types)
    {
        mangled_name += get_mangled_type_str(type);
    }

    if (auto unmangled = unmangled_names.find(mangled_name); unmangled != unmangled_names.end())
        return unmangled->second;

    return mangled_name;
}

void FunctionSignatureManager::add_unmangled_name(const std::string& name, const std::vector<llvm::Type*>& arg_types)
{
    //Use the mangled name as a key
    std::string mangled_name = get_function_name(name, arg_types);
    unmangled_names.emplace(mangled_name, name);
}

