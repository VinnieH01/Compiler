#pragma once

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

    inline E& get_error() { return error; }

    bool is_error() const { return b_error; }

private:
    E error;
    bool b_error;
};
