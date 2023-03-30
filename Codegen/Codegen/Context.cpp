#include "Context.h"
#include <llvm/Support/raw_ostream.h>

llvm::LLVMContext context;

llvm::LLVMContext& get_context()
{
    return context;
}
