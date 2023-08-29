#include <iostream>
#include <fstream>
#include <sstream>
// https://www.youtube.com/watch?v=8PpARII2ytM
#include "tokenization.h"
#include "parser.h"
#include "generation.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Invalid number of arguments" << std::endl;
        std::cerr << "Usage: " << argv[0] << " <input file.lud>" << std::endl;
        return 1;
    }

    std::string content;
    {
        std::stringstream inp_content;
        std::fstream inp(argv[1], std::ios::in);
        inp_content << inp.rdbuf();
        content = inp_content.str();
    }
    Tokenizer tokenizer(std::move(content));
    auto toks = tokenizer.tokenize();
    Parser parser(std::move(toks));
    auto tree = parser.parse_prog();
    if (!tree.has_value()) {
        std::cerr << "ERR" << std::endl;
        exit(1);
    }
    Generator generator(std::move(tree.value()));
    auto code = generator.gen_prog();
    std::cout << code << std::endl;

    std::ofstream ofile("out.asm");
    ofile << code;
    ofile.close();


    system("nasm -felf64 -o out.o out.asm");
    system("ld -o out out.o");

    return 0;
}
