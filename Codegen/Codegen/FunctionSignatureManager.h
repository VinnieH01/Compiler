#pragma once

#include <string>
#include <vector>
#include <llvm/IR/Type.h>
#include <map>

class FunctionSignatureManager
{
public:
	std::string get_function_name(const std::string& name, const std::vector<llvm::Type*>& arg_types);
	void add_unmangled_name(const std::string& name, const std::vector<llvm::Type*>& arg_types);
private:
	std::string get_mangled_type_str(llvm::Type* type);
	std::map<std::string, std::string> unmangled_names;
};
