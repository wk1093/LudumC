#pragma once

#include <utility>
#include <variant>
#include "arena.h"
#include "tokenization.h"

#define peekhas(i) peek(i).has_value()
#define peekval(i) peek(i).value()
#define peekc(i) peek(i).has_value() && peek(i).value()
#define aalloc(T, ...) (T::_new(&m_allocator,##__VA_ARGS__))

#define new_falc(T, body, ...) static T* _new(ArenaAllocator* m_allocator,##__VA_ARGS__) { auto* x = m_allocator->alloc<T>(); body; return x; }

// TODO: clean up this mess of macros and structs

struct NodeTermIntLit {
    Token int_lit;

    new_falc(NodeTermIntLit, x->int_lit = std::move(int_lit), Token int_lit);
};

struct NodeTermIdent {
    Token ident;

    new_falc(NodeTermIdent, x->ident = std::move(ident), Token ident);
};

struct NodeExpr;

#define new_binex(name)\
struct name {\
NodeExpr* lhs;\
NodeExpr* rhs;\
\
new_falc(name, x->lhs = lhs; x->rhs = rhs, NodeExpr* lhs, NodeExpr* rhs);\
new_falc(name, x->lhs = lhs; x->rhs = nullptr, NodeExpr* lhs);\
new_falc(name, x->lhs = nullptr; x->rhs = nullptr);\
};

new_binex(NodeBinExprAdd)
new_binex(NodeBinExprMul)
new_binex(NodeBinExprSub)
new_binex(NodeBinExprDiv)
// TODO: more bin exprs
// TODO: correct order of operations (how tf i do this)

struct NodeBinExpr {
    std::variant<NodeBinExprAdd*, NodeBinExprMul*, NodeBinExprSub*, NodeBinExprDiv*> var;

    new_falc(NodeBinExpr, x->var = var, NodeBinExprAdd *var);
    new_falc(NodeBinExpr, x->var = var, NodeBinExprMul *var);
    new_falc(NodeBinExpr, x->var = var, NodeBinExprSub *var);
    new_falc(NodeBinExpr, x->var = var, NodeBinExprDiv *var);

    new_falc(NodeBinExpr,);
};

struct NodeTerm {
    std::variant<NodeTermIntLit *, NodeTermIdent *> var;

    new_falc(NodeTerm, x->var = var, NodeTermIntLit *var);
    new_falc(NodeTerm, x->var = var, NodeTermIdent *var);
};

struct NodeExpr {
    std::variant<NodeTerm *, NodeBinExpr *> var;

    new_falc(NodeExpr, x->var = var, NodeTerm *var);
    new_falc(NodeExpr, x->var = var, NodeBinExpr *var);
};

struct NodeStmtExit {
    NodeExpr *expr;

    new_falc(NodeStmtExit, x->expr = expr, NodeExpr *expr);
    new_falc(NodeStmtExit, x->expr = nullptr);
};

struct NodeStmtLet {
    Token ident;
    NodeExpr *expr{};

    new_falc(NodeStmtLet, x->ident = std::move(ident); x->expr = expr, Token ident, NodeExpr *expr);
    new_falc(NodeStmtLet, x->ident = std::move(ident); x->expr = nullptr, Token ident);
};

struct NodeStmt {
    std::variant<NodeStmtExit *, NodeStmtLet *> var;

    new_falc(NodeStmt, x->var = var, NodeStmtExit *var);
    new_falc(NodeStmt, x->var = var, NodeStmtLet *var);
};

struct NodeProg {
    std::vector<NodeStmt *> stmts;
};

class Parser {
public:
    explicit Parser(std::vector<Token> tokens) : m_tokens(std::move(tokens)), m_allocator(1024 * 1024 * 4) {} // 4 MB

    std::optional<NodeTerm *> parse_term() {
        if (peekc().type == TokenType::l_int) {
            auto term_int_lit = aalloc(NodeTermIntLit, consume());
            return aalloc(NodeTerm, term_int_lit);
        } else if (peekc().type == TokenType::ident) {
            auto term_ident = aalloc(NodeTermIdent, consume());
            return aalloc(NodeTerm, term_ident);
        } else {
            return std::nullopt;
        }
    }

#define parse_bin_expr(name, typen) auto bin_expr = aalloc(NodeBinExpr);\
    auto lhs_ex = aalloc(NodeExpr, term.value());\
    auto bin_expr_ = aalloc(typen, lhs_ex);\
    consume();\
    if (auto rhs = parse_expr()) {\
        bin_expr_->rhs = rhs.value();\
        bin_expr->var = bin_expr_;\
        return aalloc(NodeExpr, bin_expr);\
    } else {\
        std::cerr << "Invalid expression in name" << #name << std::endl;\
        std::cerr << "got token type " << (int) peekval().type << std::endl;\
        exit(1);\
    }

    std::optional<NodeExpr *> parse_expr() {
        if (auto term = parse_term()) {
            if (peekc().type == TokenType::c_plus) {
                parse_bin_expr(add, NodeBinExprAdd);
            } else if (peekc().type == TokenType::c_minus) {
                parse_bin_expr(minus, NodeBinExprSub);
            } else if (peekc().type == TokenType::c_star) {
                parse_bin_expr(mul, NodeBinExprMul);
            } else if (peekc().type == TokenType::c_slash) {
                parse_bin_expr(div, NodeBinExprDiv);
            } else {
                return aalloc(NodeExpr, term.value());
            }
        }
        return std::nullopt;
    }

#define check_consume(ttype, msg) if (peekc().type == ttype ) { consume(); } else {std::cerr << (msg) << std::endl;exit(1);}


    std::optional<NodeStmt *> parse_stmt() {
        if (peekc().type == TokenType::b_exit && peekc(1).type == TokenType::c_lparen) {
            consume();
            consume();
            auto *stmt_exit = aalloc(NodeStmtExit);
            if (auto node_expr = parse_expr()) {
                stmt_exit->expr = node_expr.value();
            } else {
                std::cerr << "Invalid expression in exit" << std::endl;
                std::cerr << "got token type " << (int) peekval().type << std::endl;
                exit(1);
            }
            check_consume(TokenType::c_rparen, "Expected ')' after exit");
            check_consume(TokenType::c_semi, "Expected ';' after exit");
            return aalloc(NodeStmt, stmt_exit);
        } else if (peekc().type == TokenType::k_let && peekc(1).type == TokenType::ident &&
                   peekc(2).type == TokenType::c_eq) {
            consume();
            auto *stmt_let = aalloc(NodeStmtLet, consume());
            consume();
            if (auto node_expr = parse_expr()) {
                stmt_let->expr = node_expr.value();
            } else {
                std::cerr << "Invalid expression in let" << std::endl;
                std::cerr << "got token type " << (int) peekval().type << std::endl;
                exit(1);
            }
            check_consume(TokenType::c_semi, "Expected ';' after let");
            return aalloc(NodeStmt, stmt_let);
        } else {
            std::cerr << "Invalid statement" << std::endl;
            std::cerr << "got token type " << (int) peekval().type << std::endl;
            exit(1);
        }
    }

    std::optional<NodeProg> parse_prog() {
        NodeProg prog;
        while (peekhas()) {
            if (auto stmt = parse_stmt()) {
                prog.stmts.push_back(stmt.value());

            } else {
                std::cerr << "Invalid statement in program" << std::endl;
                std::cerr << "got token type " << (int) peekval().type << std::endl;
                exit(1);
            }
        }
        return prog;
    }

private:
    [[nodiscard]] std::optional<Token> peek(int off = 0) const {
        if (m_index + off >= m_tokens.size()) {
            return std::nullopt;
        }
        return m_tokens.at(m_index + off);
    }

    Token consume() {
        return m_tokens.at(m_index++);
    }

    const std::vector<Token> m_tokens;
    size_t m_index = 0;
    ArenaAllocator m_allocator;
};