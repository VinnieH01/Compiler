#include "ASTNodes.hpp"
#include "../Common.h"
#include "ASTFromJson.h"

static std::unique_ptr<ASTNode> node_from_type(const nlohmann::json& json) 
{
    if (json.at("type") == "Function") return std::make_unique<FunctionNode>(json, node_from_type(json.at("prototype")), create_node_list(json.at("body")));
    if (json.at("type") == "Prototype") return std::make_unique<PrototypeNode>(json);
    if (json.at("type") == "Binary") return std::make_unique<BinaryNode>(json, node_from_type(json.at("left")), node_from_type(json.at("right")));
    if (json.at("type") == "Literal") return std::make_unique<LiteralNode>(json);
    if (json.at("type") == "Variable") return std::make_unique<VariableNode>(json);
    if (json.at("type") == "Call") return std::make_unique<CallNode>(json, create_node_list(json.at("args")));
    if (json.at("type") == "Unary") return std::make_unique<UnaryNode>(json, node_from_type(json.at("operand")));
    if (json.at("type") == "Return") return std::make_unique<ReturnNode>(json, node_from_type(json.at("value")));
    if (json.at("type") == "Let") return std::make_unique<LetNode>(json, node_from_type(json.at("value")));
    if (json.at("type") == "If") return std::make_unique<IfNode>(json, node_from_type(json.at("condition")), create_node_list(json.at("then")), create_node_list(json.at("else")));
    if (json.at("type") == "Loop") return std::make_unique<LoopNode>(json, create_node_list(json.at("body")));
    if (json.at("type") == "LoopTermination") return std::make_unique<LoopTerminationNode>(json);
    if (json.at("type") == "Cast") return std::make_unique<CastNode>(json, node_from_type(json.at("value")));
    if (json.at("type") == "Dereference") return std::make_unique<DereferenceNode>(json, node_from_type(json.at("variable")));
    if (json.at("type") == "StructInstance") return std::make_unique<StructInstanceNode>(json, create_node_list(json.at("members")));
    if (json.at("type") == "StructDefinition") return std::make_unique<StructDefinitionNode>(json);

    error("Cannot create AST node from: " + (std::string)json.at("type"));
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