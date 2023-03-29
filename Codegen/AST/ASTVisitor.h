#pragma once

#include <llvm/IR/Value.h>

template<typename T>
class ASTVisitor 
{
public:
    virtual T visit_literal_node(const class LiteralNode&) = 0;
    virtual T visit_variable_node(const class VariableNode&) = 0;
    virtual T visit_let_node(const class LetNode&) = 0;
    virtual T visit_binary_node(const class BinaryNode&) = 0;
    virtual T visit_unary_node(const class UnaryNode&) = 0;
    virtual T visit_return_node(const class ReturnNode&) = 0;
    virtual T visit_prototype_node(const class PrototypeNode&) = 0;
    virtual T visit_function_node(const class FunctionNode&) = 0;
    virtual T visit_call_node(const class CallNode&) = 0;
    virtual T visit_if_node(const class IfNode&) = 0;
    virtual T visit_loop_node(const class LoopNode&) = 0;
    virtual T visit_loop_termination_node(const class LoopTerminationNode&) = 0;
    virtual T visit_cast_node(const class CastNode&) = 0;
    virtual T visit_dereference_node(const class DereferenceNode&) = 0;
    virtual T visit_struct_instance_node(const class StructInstanceNode&) = 0;
    virtual T visit_struct_definition_node(const class StructDefinitionNode&) = 0;
};
