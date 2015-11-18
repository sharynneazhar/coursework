#ifndef PRECONDITIONVIOLATIONEXCEPTION_H
#define PRECONDITIONVIOLATIONEXCEPTION_H

#include <stdexcept>
#include <string>

class PreconditionViolationException : public std::runtime_error
{
public:
    PreconditionViolationException(const char* msg);
};

#endif
