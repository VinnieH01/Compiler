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
#include "Codegen/CodegenVisitor.h"
#include "Common.h"

#include "AST/ASTFromJson.h"

using namespace llvm;
using namespace nlohmann;

int main(int argc, char* argv[])
{
    if (argc != 2) {
        std::cout << "Incorrect # of arguments";
        return -1;
    }

    std::ifstream f(argv[1]);
    std::vector<json> data = json::parse(f);

    std::vector<std::unique_ptr<ASTNode>> nodes(create_node_list(data));

    CodegenVisitor gen;

    bool gen_error = false;
    for (const auto& node : nodes)
    {
        auto res = node->accept(gen);
        if (res.is_error())
        {
            gen_error = true;
            outs() << res.get_error()->get_full_message();
        }
    }

    if (!gen_error) 
    {
        const Module& module = gen.get_module();

        module.print(outs(), nullptr);

        std::error_code EC;
        raw_fd_ostream OS("module", EC);
        WriteBitcodeToFile(module, OS);
        OS.flush();
    }
	
    return 0;
}