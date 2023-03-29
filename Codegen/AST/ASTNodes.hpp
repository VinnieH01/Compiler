#pragma once

#include "../third-party/json.hpp"

#include "ASTVisitor.h"

#define GETTER(value, type) inline type get_##value##() const { return data.at(#value); }
#define TMPLGETTER(value) template<typename T> inline T get_##value##() const { return (T)data.at(#value); }

class ASTNode 
{
public:
	ASTNode(const nlohmann::json& data)
		: data(data) {}

	GETTER(type, std::string)

	template<typename T>
	T accept(ASTVisitor<T>& v) const;

protected:
	const nlohmann::json data;
};

class LiteralNode : public ASTNode 
{
public:
	using ASTNode::ASTNode;
	TMPLGETTER(value)
	GETTER(name, std::string)
	GETTER(data_type, std::string)

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_literal_node(*this);
	};
};

class VariableNode : public ASTNode
{
public:
	using ASTNode::ASTNode;
	GETTER(name, std::string)

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_variable_node(*this);
	};
};

class BinaryNode : public ASTNode
{
public:
	BinaryNode(const nlohmann::json& data, std::unique_ptr<ASTNode> left, std::unique_ptr<ASTNode> right)
		: ASTNode(data),
		left(std::move(left)),
		right(std::move(right)) {}

	GETTER(operator, std::string)

	inline const std::unique_ptr<ASTNode>& get_left() const { return left; }
	inline const std::unique_ptr<ASTNode>& get_right() const { return right; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_binary_node(*this);
	};
private:
	std::unique_ptr<ASTNode> left;
	std::unique_ptr<ASTNode> right;
};

class CallNode : public ASTNode
{
public:
	CallNode(const nlohmann::json& data, std::vector<std::unique_ptr<ASTNode>> args)
		: ASTNode(data),
		args(std::move(args)) {}
	GETTER(callee, std::string)
	inline const std::vector<std::unique_ptr<ASTNode>>& get_args() const { return args; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_call_node(*this);
	};
private:
	std::vector<std::unique_ptr<ASTNode>> args;
};

class UnaryNode : public ASTNode
{
public:
	UnaryNode(const nlohmann::json& data, std::unique_ptr<ASTNode> operand)
		: ASTNode(data),
		operand(std::move(operand)) {}

	GETTER(operator, std::string)

	inline const std::unique_ptr<ASTNode>& get_operand() const { return operand; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_unary_node(*this);
	};
private:
	std::unique_ptr<ASTNode> operand;
};

class PrototypeNode : public ASTNode
{
public:
	using ASTNode::ASTNode;

	GETTER(name, std::string)
	GETTER(ret_type, std::string)
	GETTER(is_extern, bool)
	GETTER(arg_types, std::vector<std::string>)
	GETTER(args, std::vector<std::string>)

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_prototype_node(*this);
	};
};

class FunctionNode : public ASTNode
{
public:
	FunctionNode(const nlohmann::json& data, std::unique_ptr<ASTNode> prototype, std::vector<std::unique_ptr<ASTNode>> body)
		: ASTNode(data),
		prototype(std::move(prototype)),
		body(std::move(body)) {}

	inline const PrototypeNode* const get_prototype() const  { return (PrototypeNode*)prototype.get(); }
	inline const std::vector<std::unique_ptr<ASTNode>>& get_body() const { return body; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_function_node(*this);
	};
private:
	std::unique_ptr<ASTNode> prototype;
	std::vector<std::unique_ptr<ASTNode>> body;
};

class ReturnNode : public ASTNode
{
public:
	ReturnNode(const nlohmann::json& data, std::unique_ptr<ASTNode> value)
		: ASTNode(data),
		value(std::move(value)) {}

	inline const std::unique_ptr<ASTNode>& get_value() const { return value; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_return_node(*this);
	};
private:
	std::unique_ptr<ASTNode> value;
};

class LetNode : public ASTNode
{
public:
	LetNode(const nlohmann::json& data, std::unique_ptr<ASTNode> value)
		: ASTNode(data),
		value(std::move(value)) {}

	GETTER(name, std::string)
	GETTER(data_type, std::string)

	inline const std::unique_ptr<ASTNode>& get_value() const { return value; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_let_node(*this);
	};
private:
	std::unique_ptr<ASTNode> value;
};

class IfNode : public ASTNode
{
public:
	IfNode(const nlohmann::json& data, std::unique_ptr<ASTNode> condition, std::vector<std::unique_ptr<ASTNode>> then, std::vector<std::unique_ptr<ASTNode>> else_)
		: ASTNode(data),
		then(std::move(then)),
		else_(std::move(else_)),
		condition(std::move(condition)) {}

	inline const std::unique_ptr<ASTNode>& get_condition() const { return condition; }
	inline const std::vector<std::unique_ptr<ASTNode>>& get_then() const { return then; }
	inline const std::vector<std::unique_ptr<ASTNode>>& get_else() const { return else_; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_if_node(*this);
	};
private:
	std::unique_ptr<ASTNode> condition;
	std::vector<std::unique_ptr<ASTNode>> then;
	std::vector<std::unique_ptr<ASTNode>> else_;
};

class LoopNode : public ASTNode
{
public:
	LoopNode(const nlohmann::json& data, std::vector<std::unique_ptr<ASTNode>> body)
		: ASTNode(data),
		body(std::move(body)) {}

	inline const std::vector<std::unique_ptr<ASTNode>>& get_body() const { return body; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_loop_node(*this);
	};
private:
	std::vector<std::unique_ptr<ASTNode>> body;
};

class LoopTerminationNode : public ASTNode
{
public:
	using ASTNode::ASTNode;
	GETTER(break, bool)

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_loop_termination_node(*this);
	};
};

class CastNode : public ASTNode
{
public:
	CastNode(const nlohmann::json& data, std::unique_ptr<ASTNode> value)
		: ASTNode(data),
		value(std::move(value)) {}

	GETTER(data_type, std::string)

	inline const std::unique_ptr<ASTNode>& get_value() const { return value; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_cast_node(*this);
	};
private:
	std::unique_ptr<ASTNode> value;
};

class DereferenceNode : public ASTNode
{
public:
	DereferenceNode(const nlohmann::json& data, std::unique_ptr<ASTNode> variable)
		: ASTNode(data),
		variable(std::move(variable)) {}

	GETTER(data_type, std::string)

	inline const std::unique_ptr<ASTNode>& get_variable() const { return variable; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_dereference_node(*this);
	};
private:
	std::unique_ptr<ASTNode> variable;
};

class StructInstanceNode : public ASTNode
{
public:
	StructInstanceNode(const nlohmann::json& data, std::vector<std::unique_ptr<ASTNode>> members)
		: ASTNode(data),
		members(std::move(members)) {}

	GETTER(data_type, std::string)

	inline const std::vector<std::unique_ptr<ASTNode>>& get_members() const { return members; }

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_struct_instance_node(*this);
	};
private:
	std::vector<std::unique_ptr<ASTNode>> members;
};

class StructDefinitionNode : public ASTNode
{
public:
	using ASTNode::ASTNode;

	GETTER(name, std::string)
	GETTER(member_types, std::vector<std::string>)
	GETTER(member_names, std::vector<std::string>)

	template<typename T>
	inline T accept(ASTVisitor<T>& v) const
	{
		return v.visit_struct_definition_node(*this);
	};
};

#undef GETTER
#undef TMPLGETTER

template<typename T>
inline T ASTNode::accept(ASTVisitor<T>& v) const
{
	//This is to allow the ASTVisitor to have a templated return type, as virtual methods cant be templated
	//ASTNode::accept cannot be virtual. Instead we need some sort of dispatch to handle the call

	//TODO: Possible alternative solution is to return void and make the visitor subclass itself
	//Save the value it wants to "return" or if template functionality is desired 
	//https://stackoverflow.com/a/65287099 can be a solution

	#define dispatch(node_type) if (get_type() == #node_type) return static_cast<const node_type##Node*>(this)->accept(v)

	dispatch(Literal);
	dispatch(Function);
	dispatch(Prototype);
	dispatch(Binary);
	dispatch(Variable);
	dispatch(Call);
	dispatch(Unary);
	dispatch(Return);
	dispatch(Let);
	dispatch(If);
	dispatch(Loop);
	dispatch(LoopTermination);
	dispatch(Cast);
	dispatch(Dereference);
	dispatch(StructInstance);
	dispatch(StructDefinition);

	#undef dispatch
}
