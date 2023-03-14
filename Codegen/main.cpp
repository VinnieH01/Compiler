#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

#include "json.hpp"
#include "IRGenerator.h"

using namespace llvm;
using namespace nlohmann;

int main(int argc, char* argv[])
{
    if (argc != 2) {
        std::cout << "Incorrect # of arguments";
        return -1;
    }

    std::unique_ptr<LLVMContext> ctx;
    std::unique_ptr<Module> module;
    std::unique_ptr<IRBuilder<>> builder;
    std::unique_ptr<legacy::FunctionPassManager> fpm;

    ctx = std::make_unique<LLVMContext>();
    module = std::make_unique<Module>("Module", *ctx);
    builder = std::make_unique<IRBuilder<>>(*ctx);

    fpm = std::make_unique<legacy::FunctionPassManager>(module.get());

    // Promote allocas to registers.
    fpm->add(createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    fpm->add(createInstructionCombiningPass());
    // Reassociate expressions.
    fpm->add(createReassociatePass());
    // Eliminate Common SubExpressions.
    fpm->add(createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    fpm->add(createCFGSimplificationPass());

    fpm->doInitialization();

    IRGenerator gen(ctx, module, builder, fpm);

    std::ifstream f(argv[1]);
    std::vector<json> data = json::parse(f);

    for (const json& data0 : data)
    {
        gen.visit_node(data0);
    }

	module->print(outs(), nullptr);

    std::error_code EC;
    raw_fd_ostream OS("module", EC);
    WriteBitcodeToFile(*module, OS);
    OS.flush();
	
    return 0;
}