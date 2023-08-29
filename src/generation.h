#pragma once

#include <utility>
#include <variant>
#include <unordered_map>
#include "parser.h"

class Generator {
public:
    explicit Generator(NodeProg prog) : m_prog(std::move(prog)) {}

    void gen_expr(const NodeExpr* expr) {
        struct ExprVisitor {
            Generator* gen;
            explicit ExprVisitor(Generator* gen) : gen(gen) {}

            void operator()(const NodeExprIntLit* int_lit) const {
                gen->m_output << "mov rax, " << int_lit->int_lit.value.value() << "\n";
                gen->push("rax");
            }
            void operator()(const NodeExprIdent* ident) const {
                if (!gen->m_vars.contains(ident->ident.value.value())) {
                    std::cerr << "ERR: Variable '" << ident->ident.value.value() << "' does not exist" << std::endl;
                    exit(1);
                }
                gen->push("QWORD [rsp+" + std::to_string((gen->m_stack_size-gen->m_vars.at(ident->ident.value.value()).stack_loc - 1) * 8) + "]\n");

            }
            void operator()(const NodeBinExpr* bin_expr_add) const {
                std::cerr << "ERR: Not implemented" << std::endl;
                exit(1);
            }
        };

        ExprVisitor visitor(this);
        std::visit(visitor, expr->var);
    }

    void gen_stmt(const NodeStmt* stmt) {
        struct StmtVisitor {
            Generator* gen;
            explicit StmtVisitor(Generator* gen) : gen(gen) {}

            void operator()(const NodeStmtExit* stmt_exit) const {
                gen->gen_expr(stmt_exit->expr);
                gen->m_output << "mov rax, 60\n";
                gen->pop("rdi");
                gen->m_output << "syscall\n";
            }
            void operator()(const NodeStmtLet* stmt_let) const {
                if (gen->m_vars.contains(stmt_let->ident.value.value())) {
                    std::cerr << "ERR: Variable '" << stmt_let->ident.value.value() << "' already exists" << std::endl;
                    exit(1);
                }
                gen->m_vars.insert({stmt_let->ident.value.value(), Var{.stack_loc = gen->m_stack_size}});
                gen->gen_expr(stmt_let->expr);
            }
        };

        StmtVisitor visitor(this);
        std::visit(visitor, stmt->var);
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