#pragma once

#include "Result.h"
#include "LangError.hpp"
#include <llvm/IR/Value.h>

using ValueResult = Result<llvm::Value*, LangError>;
