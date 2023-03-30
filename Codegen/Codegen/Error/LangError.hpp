#pragma once
#include <string>
#include <vector>
#include <memory>
#include <variant>

using ErrorVariant = std::variant<class CustomError, class InvalidSymbolError, class RedeclarationError, class IllegalOperationError, class IncompatibleTypeError, class LangErrorList>;

/*class DefaultError
{
public:
    //This only exists to give the variant a default constructor
    inline std::string get_full_message() const
    {
        //We should never be in a position where we access a default error
        assert(false);
        return "";
    }
};*/

class CustomError
{
public:
    CustomError(const std::string& message)
        : message(message) {}

    inline std::string get_full_message() const
    {
        return message;
    }
private:
    const std::string message;
};

class InvalidSymbolError
{
public:
    InvalidSymbolError(const std::string& symbol)
        : message("Invalid symbol")
        , symbol_name(symbol) {}

    inline std::string get_full_message() const
    {
        return message + ": " + symbol_name;
    }
private:
    std::string symbol_name;
    const std::string message;
};

class RedeclarationError 
{
public:
    RedeclarationError(const std::string& symbol)
        : message("Cannot redeclare symbol")
        , symbol_name(symbol) {}

    inline std::string get_full_message() const
    {
        return message + ": " + symbol_name;
    }
private:
    std::string symbol_name;
    const std::string message;
};

class IllegalOperationError
{
public:
    IllegalOperationError(const std::string& operation)
        : message("Illegal operation")
        , operation(operation) {}

    inline std::string get_full_message() const
    {
        return message + ": " + operation;
    }
private:
    std::string operation;
    const std::string message;
};

class IncompatibleTypeError
{
public:
    IncompatibleTypeError(const std::string& operation)
        : message("Incompatible types")
        , operation(operation) {}

    inline std::string get_full_message() const
    {
        return message + " in operation: " + operation;
    }
private:
    std::string operation;
    const std::string message;
};

std::string get_error_message(const ErrorVariant& err);

class LangErrorList
{
public:
    inline void add_error(const ErrorVariant& error)
    {
        errors.push_back(error);
    }

    inline bool has_errors() { return errors.size() > 0; }

    inline std::string get_full_message() const
    {
        std::string message;
        for (const auto& err : errors)
        {
            message += ::get_error_message(err) + '\n';
        }
        return message;
    }
private:
    std::vector<ErrorVariant> errors;
};

inline std::string get_error_message(const ErrorVariant& err)
{
    auto call = [](const auto& err) { return err.get_full_message(); };
    return std::visit(call, err);
}

class LangError
{
public:
    LangError(const CustomError& e) : err(e) {}
    LangError(const InvalidSymbolError& e) : err(e) {}
    LangError(const RedeclarationError& e) : err(e) {}
    LangError(const IllegalOperationError& e) : err(e) {}
    LangError(const IncompatibleTypeError& e) : err(e) {}
    LangError(const LangErrorList& e) : err(e) {}

    LangError(const std::string& m) : err(CustomError(m)) {}
    LangError() : err(CustomError("")) {}

    inline std::string get_error_message() { return ::get_error_message(err); };
    inline const ErrorVariant& get_variant() { return err; }
private:
    const ErrorVariant err;
};