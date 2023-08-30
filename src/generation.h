#pragma once

#include <utility>
#include <variant>
#include <unordered_map>
#include "parser.h"

// TODO: get rid of this godawful syntactic mess
// why have I done this
#define mk_vis_b() struct _MyVisitor { Generator* gen; explicit _MyVisitor(Generator* gen) : gen(gen) {}
#define mk_vis_e(input) }; _MyVisitor _vis(this); std::visit(_vis, input);
#define vis(tdec) void operator()(const tdec* i) const
class Generator {
public:
    explicit Generator(NodeProg prog) : m_prog(std::move(prog)) {}

    void gen_term(const NodeTerm* term) {
        mk_vis_b()
           vis(NodeTermIntLit) {
               gen->m_output << "mov rax, " << i->int_lit.value.value() << "\n";
               gen->push("rax");
           }
           vis(NodeTermIdent) {
               if (!gen->m_vars.contains(i->ident.value.value())) {
                   std::cerr << "ERR: Variable '" << i->ident.value.value() << "' does not exist" << std::endl;
                   exit(1);
               }
               gen->push("QWORD [rsp+" + std::to_string((gen->m_stack_size-gen->m_vars.at(i->ident.value.value()).stack_loc - 1) * 8) + "]");
           }
        mk_vis_e(term->var)
    }

    void gen_bin_expr(const NodeBinExpr* binexpr) {
        mk_vis_b()
            vis(NodeBinExprAdd) {
                gen->gen_expr(i->lhs);
                gen->gen_expr(i->rhs);
                gen->pop("rdi");
                gen->pop("rax");
                gen->m_output << "add rax, rdi\n";
                gen->push("rax");
            }
            vis(NodeBinExprSub) {
                gen->gen_expr(i->lhs);
                gen->gen_expr(i->rhs);
                gen->pop("rdi");
                gen->pop("rax");
                gen->m_output << "sub rax, rdi\n";
                gen->push("rax");

            }
            vis(NodeBinExprDiv) {
                std::cout << "Dividing not implemented" << std::endl;
                exit(1);
            }
            vis(NodeBinExprMul) {
                std::cout << "Multiplying not implemented" << std::endl;
                exit(1);
            }
        mk_vis_e(binexpr->var)
    }

    void gen_expr(const NodeExpr* expr) {
        mk_vis_b()
            vis(NodeTerm) {
                gen->gen_term(i);
            }
            vis(NodeBinExpr) {
                gen->gen_bin_expr(i);
            }
        mk_vis_e(expr->var)
    }

    void gen_stmt(const NodeStmt* stmt) {
        mk_vis_b()
            vis(NodeStmtExit) {
                gen->m_output << "; exit\n";
                gen->gen_expr(i->expr);
                gen->m_output << "mov rax, 60\n";
                gen->pop("rdi");
                gen->m_output << "syscall\n";
            }
            vis(NodeStmtLet) {
                gen->m_output << "; let\n";
                if (gen->m_vars.contains(i->ident.value.value())) {
                    std::cerr << "ERR: Variable '" << i->ident.value.value() << "' already exists" << std::endl;
                    exit(1);
                }
                gen->m_vars.insert({i->ident.value.value(), Var{.stack_loc = gen->m_stack_size}});
                gen->gen_expr(i->expr);
            }
        mk_vis_e(stmt->var)
    }

    [[nodiscard]] std::string gen_prog() {
        std::stringstream output;
        m_output << "global _start\n_start:\n";

        for (const NodeStmt* stmt : m_prog.stmts) {
            gen_stmt(stmt);
        }

        m_output << "mov rax, 60\n";
        m_output << "mov rdi, 0\n";
        m_output << "syscall\n";
        return m_output.str();
    }
private:
    void push(const std::string& reg) {
        m_output << "push " << reg << "\n";
        m_stack_size++;
    }
    void pop(const std::string& reg) {
        m_output << "pop " << reg << "\n";
        m_stack_size--;
    }

    struct Var {
        size_t stack_loc;
        // TODO: type
    };

    const NodeProg m_prog;
    std::stringstream m_output;
    size_t m_stack_size = 0;
    std::unordered_map<std::string, Var> m_vars{};
};