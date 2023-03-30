#include "StructManager.h"

using namespace llvm;

void StructManager::add_struct(llvm::StructType* type, const std::vector<std::string>& field_names)
{
	struct_fields.emplace(type, field_names);
}

Result<unsigned, LangError> StructManager::get_field_index(llvm::StructType* type, const std::string& field_name)
{
    if (const auto& field_names = struct_fields.find(type); field_names != struct_fields.end())
    {
        if (auto field_it = std::find(field_names->second.begin(), field_names->second.end(), field_name); field_it != field_names->second.end())
        {
            return field_it - field_names->second.begin();
        }
    }
    return LangError("Cannot get field '" + field_name + "' from struct");
}
