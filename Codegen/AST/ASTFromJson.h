#pragma once
#include <vector>
#include <memory>
#include "ASTNodes.hpp"

std::vector<std::unique_ptr<ASTNode>> create_node_list(const nlohmann::json& json_tree);
