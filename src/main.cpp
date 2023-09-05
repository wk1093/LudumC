#include <iostream>
#include <fstream>
#include <sstream>
#include <cstring>
#include "tokenization.h"
#include "parser.h"
#include "generation.h"
// https://youtu.be/6nl5HTGgvnk?si=e177bxcrGldEJujs&t=2690
// TODO: when done, split .h into .h and .cpp

bool secure_system(const char* str) {
    int status = system(str);
    if (status < 0)
        std::cout << "ERR" << strerror(errno) << std::endl;
    else {
        if (WIFEXITED(status)) {
            if (WEXITSTATUS(status) != 0) {
                std::cout << "ERR " << WEXITSTATUS(status) << std::endl;
            } else {
                return false;
            }
        }
        else
            std::cout << "Exited abnormally" << std::endl;
    }
    return true;
}

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
//    std::cout << code << std::endl;

    std::ofstream ofile("out.asm");
    ofile << code;
    ofile.close();


    if (!secure_system("nasm -felf64 -o out.o out.asm"))
        secure_system("ld -o out out.o");

    return 0;
}
