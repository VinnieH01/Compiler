#pragma once
#include <string>
#include <vector>
#include <memory>

class LangError
{
public:
    LangError(const std::string& message) : message(message) {}
    inline const std::string& get_message() const { return message; }
    inline virtual std::string get_full_message() const { return get_message(); };
protected:
    const std::string message;
};

class InvalidSymbolError : public LangError
{
public:
    InvalidSymbolError(const std::string& symbol)
        : LangError("Invalid symbol")
        , symbol_name(symbol) {}

    inline std::string get_full_message() const override
    {
        return message + ": " + symbol_name;
    }
private:
    std::string symbol_name;
};

class RedeclarationError : public LangError
{
public:
    RedeclarationError(const std::string& symbol)
        : LangError("Cannot redeclare symbol")
        , symbol_name(symbol) {}

    inline std::string get_full_message() const override
    {
        return message + ": " + symbol_name;
    }
private:
    std::string symbol_name;
};

class IllegalOperationError : public LangError
{
public:
    IllegalOperationError(const std::string& operation)
        : LangError("Illegal operation")
        , operation(operation) {}

    inline std::string get_full_message() const override
    {
        return message + ": " + operation;
    }
private:
    std::string operation;
};

class IncompatibleTypeError : public LangError
{
public:
    IncompatibleTypeError(const std::string& operation)
        : LangError("Incompatible types")
        , operation(operation) {}

    inline std::string get_full_message() const override
    {
        return message + " in operation: " + operation;
    }
private:
    std::string operation;
};

class LangErrorList : public LangError
{
public:
    LangErrorList() : LangError("") {}

    inline void add_error(const std::shared_ptr<LangError>& error)
    {
        errors.push_back(error);
    }

    inline bool has_errors() { return errors.size() > 0; }

    inline std::string get_full_message() const override
    {
        std::string message;
        for (const auto& err : errors)
        {
            message += err->get_full_message() + '\n';
        }
        return message;
    }
private:
    std::vector<std::shared_ptr<LangError>> errors;
};