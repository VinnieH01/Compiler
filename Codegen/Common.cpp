#include "Common.h"
#include <llvm/Support/raw_ostream.h>

void error(const std::string& message)
{
    llvm::outs() << "ERROR: " << message << "\n";
    exit(-1);
}