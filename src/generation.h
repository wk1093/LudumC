#pragma once

#include <utility>
#include <variant>
#include <vector>
#include <algorithm>
#include <cassert>
#include "parser.h"

#define mk_vis_b() struct _MyVisitor { Generator& gen; explicit _MyVisitor(Generator& gen) : gen(gen) {}
#define mk_vis_e(input) }; _MyVisitor _vis(*this); std::visit(_vis, input);
#define vis(tdec) void operator()(const tdec* i) const
class Generator {
public:
    explicit Generator(NodeProg prog) : m_prog(std::move(prog)) {}

    void gen_term(const NodeExprTerm* term) {
        mk_vis_b()

            vis(NodeExprTermIntLit) {
                gen.m_output << "mov rax, " << i->int_lit.value.value() << "\n";
                gen.push("rax");
            }
            vis(NodeExprTermIdent) {
                auto it = std::find_if(gen.m_vars.cbegin(), gen.m_vars.cend(), [&](const Var& var){ return var.name == i->ident.value.value();});
                if (it == gen.m_vars.cend()) {
                    std::cerr << "ERR: Variable '" << i->ident.value.value() << "' does not exist" << std::endl;
                    exit(1);
                }
                gen.push("QWORD [rsp+" + std::to_string((gen.m_stack_size-(*it).stack_loc - 1) * 8) + "]");
            }
            vis(NodeExprTermParen) {
                gen.gen_expr(i->expr);
            }
        mk_vis_e(term->var)
    }

    void gen_bin_expr(const NodeExprBin* binexpr) {
        mk_vis_b()
            vis(NodeExprBinAdd) {
                gen.gen_expr(i->lhs);
                gen.gen_expr(i->rhs);
                gen.pop("rbx");
                gen.pop("rax");
                gen.m_output << "add rax, rbx\n";
                gen.push("rax");
            }
            vis(NodeExprBinSub) {
                gen.gen_expr(i->lhs);
                gen.gen_expr(i->rhs);
                gen.pop("rbx");
                gen.pop("rax");
                gen.m_output << "sub rax, rbx\n";
                gen.push("rax");

            }
            vis(NodeExprBinDiv) {
                gen.gen_expr(i->lhs);
                gen.gen_expr(i->rhs);
                gen.pop("rbx");
                gen.pop("rax");
                gen.m_output << "div rbx\n";
                gen.push("rax");
            }
            vis(NodeExprBinMul) {
                gen.gen_expr(i->lhs);
                gen.gen_expr(i->rhs);
                gen.pop("rbx");
                gen.pop("rax");
                gen.m_output << "mul rbx\n";
                gen.push("rax");
            }
            vis(NodeExprBinMod) {
                gen.gen_expr(i->lhs);
                gen.gen_expr(i->rhs);
                gen.pop("rbx");
                gen.pop("rax");
                gen.m_output << "div rbx\n";
                gen.push("rdx");
        }
        mk_vis_e(binexpr->var)
    }

    void gen_expr(const NodeExpr* expr) {
        mk_vis_b()
            vis(NodeExprTerm) {
                gen.gen_term(i);
            }
            vis(NodeExprBin) {
                gen.gen_bin_expr(i);
            }
        mk_vis_e(expr->var)
    }

    void gen_scope(const NodeScope* scope) {
        begin_scope();
        for (const NodeStmt* stmt : scope->stmts) {
            gen_stmt(stmt);
        }
        end_scope();
    }

    void gen_stmt(const NodeStmt* stmt) {
        mk_vis_b()
            vis(NodeStmtExit) {
                gen.m_output << "; exit\n";
                gen.gen_expr(i->expr);
                gen.m_output << "mov rax, 60\n";
                gen.pop("rdi");
                gen.m_output << "syscall\n";
            }
            vis(NodeStmtLet) {
                gen.m_output << "; let\n";
                //if (gen.m_vars.contains(i->ident.value.value())) {
                auto it = std::find_if(gen.m_vars.cbegin(), gen.m_vars.cend(), [&](const Var& var){ return var.name == i->ident.value.value();});
                if (it != gen.m_vars.cend()) {
                    std::cerr << "ERR: Variable '" << i->ident.value.value() << "' already exists" << std::endl;
                    exit(1);
                }
                gen.m_vars.push_back(Var{.name=i->ident.value.value(), .stack_loc=gen.m_stack_size});
                gen.gen_expr(i->expr);
            }
            vis(NodeScope) {
                gen.gen_scope(i);
            }
            vis(NodeStmtIf) {
                gen.m_output << "; if\n";
                gen.gen_expr(i->expr);
                gen.pop("rax");
                std::string lbl = gen.create_label();
                gen.m_output << "test rax, rax\n";
                gen.m_output << "jz " << lbl << "\n";
                gen.gen_stmt(i->stmt);
                gen.m_output << lbl << ":\n";
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

    void begin_scope() {
        m_output << "; begin_scope\n";
        m_scopes.push_back(m_vars.size());
    }

    void end_scope() {
        m_output << "; end_scope\n";
        size_t pop_count = m_vars.size() - m_scopes.back();
        m_output << "add rsp, " << pop_count * 8 << "\n";
        m_stack_size -= pop_count;
        for (int i = 0; i < pop_count; i++) {
            m_vars.pop_back();
        }
        m_scopes.pop_back();
    }

    std::string create_label() {
        return "__label_" + std::to_string(m_labels++);
    }

    struct Var {
        std::string name;
        size_t stack_loc;
        // TODO: type
    };

    const NodeProg m_prog;
    std::stringstream m_output;
    size_t m_stack_size = 0;
    std::vector<Var> m_vars;
    std::vector<size_t> m_scopes{};
    int m_labels = 0;
};