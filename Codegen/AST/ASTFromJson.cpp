#include "ASTNodes.hpp"
#include "ASTFromJson.h"
#include <iostream>

static std::unique_ptr<ASTNode> node_from_type(const nlohmann::json& json) 
{
    const std::string& type = json.at("type");

    if (type == "Function") return std::make_unique<FunctionNode>(json, node_from_type(json.at("prototype")), create_node_list(json.at("body")));
    if (type == "Prototype") return std::make_unique<PrototypeNode>(json);
    if (type == "Binary") return std::make_unique<BinaryNode>(json, node_from_type(json.at("left")), node_from_type(json.at("right")));
    if (type == "Literal") return std::make_unique<LiteralNode>(json);
    if (type == "Variable") return std::make_unique<VariableNode>(json);
    if (type == "Call") return std::make_unique<CallNode>(json, create_node_list(json.at("args")));
    if (type == "Unary") return std::make_unique<UnaryNode>(json, node_from_type(json.at("operand")));
    if (type == "Return") return std::make_unique<ReturnNode>(json, node_from_type(json.at("value")));
    if (type == "Let") return std::make_unique<LetNode>(json, node_from_type(json.at("value")));
    if (type == "If") return std::make_unique<IfNode>(json, node_from_type(json.at("condition")), create_node_list(json.at("then")), create_node_list(json.at("else")));
    if (type == "Loop") return std::make_unique<LoopNode>(json, create_node_list(json.at("body")));
    if (type == "LoopTermination") return std::make_unique<LoopTerminationNode>(json);
    if (type == "Cast") return std::make_unique<CastNode>(json, node_from_type(json.at("value")));
    if (type == "Dereference") return std::make_unique<DereferenceNode>(json, node_from_type(json.at("variable")));
    if (type == "StructInstance") return std::make_unique<StructInstanceNode>(json, create_node_list(json.at("members")));
    if (type == "StructDefinition") return std::make_unique<StructDefinitionNode>(json);

    std::cerr << "Cannot create AST node from: " + (std::string)json.at("type");
    exit(-1);
}

std::vector<std::unique_ptr<ASTNode>> create_node_list(const nlohmann::json& json_tree)
{
    std::vector<std::unique_ptr<ASTNode>> tree;
	for (const auto& json : json_tree)
	{
        tree.push_back(std::move(node_from_type(json)));
	}
    return tree;
}