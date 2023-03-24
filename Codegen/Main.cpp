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

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

#include "third-party/json.hpp"
#include "IRGenerator.h"
#include "Common.h"

using namespace llvm;
using namespace nlohmann;

int main(int argc, char* argv[])
{
    if (argc != 2) {
        std::cout << "Incorrect # of arguments";
        return -1;
    }

    std::unique_ptr<Module> module;
    std::unique_ptr<legacy::FunctionPassManager> fpm;

    module = std::make_unique<Module>("Module", get_context());

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

    IRGenerator gen(module, fpm);

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