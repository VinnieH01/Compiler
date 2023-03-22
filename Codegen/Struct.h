#pragma once

#include <string>
#include <map>
#include <vector>

class Struct
{
private:
	std::string type;
public:
	Struct(std::vector<std::string> types);
	const std::string& get_type() const;
};

