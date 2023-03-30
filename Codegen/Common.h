#pragma once

#include <string>
#include <map>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>

llvm::LLVMContext& get_context();

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

class SymbolRedeclarationError : public LangError
{
public:
    SymbolRedeclarationError(const std::string& symbol)
        : LangError("Cannot redeclare symbol")
        , symbol_name(symbol) {}

    inline std::string get_full_message() const override
    {
        return message + ": " + symbol_name;
    }
private:
    std::string symbol_name;
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

template <typename T, typename E>
class Result 
{
public:
    Result(T val) : value(val), b_error(false) {}
    Result(E err) : error(err), b_error(true) {}

    inline T& operator*() { return value; }
    inline E& get_error() { return error; }

    bool is_error() const { return b_error; }

    ~Result() 
    {
        if (b_error)
            error.~E();
        else
            value.~T();
    }

private:
    union 
    {
        T value;
        E error;
    };
    bool b_error;
};

template <typename E>
class Result<void, E>
{
public:
    Result() : b_error(false) {}
    Result(E err) : error(err), b_error(true) {}

    //inline T& operator*() { return value; }
    inline E& get_error() { return error; }

    bool is_error() const { return b_error; }

private:
    E error;
    bool b_error;
};

using ValueResult = Result<llvm::Value*, std::shared_ptr<LangError>>;