#include "Struct.h"

Struct::Struct(std::vector<std::string> types)
{
	for (const std::string& type : types)
	{
		this->type += type + ",";
	}
	type = type.substr(0, type.size() - 1); //Remove last comma
}

const std::string& Struct::get_type() const
{
	return type;
}
