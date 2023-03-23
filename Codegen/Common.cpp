#include "Common.h"
#include <llvm/Support/raw_ostream.h>

llvm::LLVMContext context;

void error(const std::string& message)
{
    llvm::outs() << "ERROR: " << message << "\n";
    exit(-1);
}

llvm::LLVMContext& get_context()
{
    return context;
}
