#pragma once
#include <unordered_map>
#include <llvm/IR/DerivedTypes.h>
#include "Error/Result.h"
#include "Error/LangError.hpp"

class StructManager 
{
public:
	void add_struct(llvm::StructType* type, const std::vector<std::string>& field_names);
	Result<unsigned, LangError> get_field_index(llvm::StructType* type, const std::string& field_name);
private:
	std::unordered_map<llvm::StructType*, std::vector<std::string>> struct_fields;
};
