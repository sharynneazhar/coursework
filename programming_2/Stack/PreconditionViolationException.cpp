#include "PreconditionViolationException.h"
#include <stdexcept>

PreconditionViolationException::PreconditionViolationException(const char* msg) : std::runtime_error(msg)
{
}
