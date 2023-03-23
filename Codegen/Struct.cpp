#include "Struct.h"
#include "GeneratorHelper.h"
#include "Common.h"

using namespace llvm;

Struct::Struct(std::vector<std::string> string_types)
{
    std::vector<Type*> types;
    for (std::string& type : string_types)
    {
        types.push_back(GeneratorHelper::get_type_from_string(type));
    }
    type = StructType::get(get_context(), types);
}

StructType* Struct::get_type() const
{
	return type;
}
