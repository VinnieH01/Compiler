#pragma once

#include <string>
#include <map>
#include <vector>
#include <llvm/IR/DerivedTypes.h>

class Struct
{
private:
	llvm::StructType* type;
public:
	Struct(std::vector<std::string> types);
	llvm::StructType* get_type() const;
};

