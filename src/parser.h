#pragma once

#include <utility>
#include <variant>
#include "arena.h"
#include "tokenization.h"

#define peekhas(i) peek(i).has_value()
#define peekval(i) peek(i).value()
#define peekc(i) peek(i).has_value() && peek(i).value()
#define peekct(i) peek(i).has_value() && peek(i).value().type
#define aalloc(T, ...) (T::_new(&m_allocator,##__VA_ARGS__))

#define new_falc(T, body, ...) static T* _new(ArenaAllocator* m_allocator,##__VA_ARGS__) { auto* x = m_allocator->alloc<T>(); body; return x; }

// TODO: clean up this mess of macros and structs

struct NodeExprTermIntLit {
    Token int_lit;

    new_falc(NodeExprTermIntLit, x->int_lit = std::move(int_lit), Token int_lit);
};

struct NodeExprTermIdent {
    Token ident;

    new_falc(NodeExprTermIdent, x->ident = std::move(ident), Token ident);
};

struct NodeExpr;

struct NodeExprTermParen {
    NodeExpr *expr;

    new_falc(NodeExprTermParen, x->expr = expr, NodeExpr *expr);
};

struct NodeExprTerm {
    std::variant<NodeExprTermIntLit *, NodeExprTermIdent *, NodeExprTermParen*> var;

    new_falc(NodeExprTerm, x->var = var, NodeExprTermIntLit *var);
    new_falc(NodeExprTerm, x->var = var, NodeExprTermIdent *var);
    new_falc(NodeExprTerm, x->var = var, NodeExprTermParen *var);
};

#define new_binex(name)\
struct name {\
NodeExpr* lhs;\
NodeExpr* rhs;\
\
new_falc(name, x->lhs = lhs; x->rhs = rhs, NodeExpr* lhs, NodeExpr* rhs);\
new_falc(name, x->lhs = lhs; x->rhs = nullptr, NodeExpr* lhs);\
new_falc(name, x->lhs = nullptr; x->rhs = nullptr);\
};

new_binex(NodeExprBinAdd)
new_binex(NodeExprBinMul)
new_binex(NodeExprBinSub)
new_binex(NodeExprBinDiv)
new_binex(NodeExprBinMod)
// TODO: more bin exprs
// TODO: correct order of operations (how tf i do this)

struct NodeExprBin {
    std::variant<NodeExprBinAdd*, NodeExprBinMul*, NodeExprBinSub*, NodeExprBinDiv*, NodeExprBinMod*> var;

    new_falc(NodeExprBin, x->var = var, NodeExprBinAdd *var);
    new_falc(NodeExprBin, x->var = var, NodeExprBinMul *var);
    new_falc(NodeExprBin, x->var = var, NodeExprBinSub *var);
    new_falc(NodeExprBin, x->var = var, NodeExprBinDiv *var);
    new_falc(NodeExprBin, x->var = var, NodeExprBinMod *var);

    new_falc(NodeExprBin,);
};

struct NodeExpr {
    std::variant<NodeExprTerm *, NodeExprBin *> var;

    new_falc(NodeExpr, x->var = var, NodeExprTerm *var);
    new_falc(NodeExpr, x->var = var, NodeExprBin *var);
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

struct NodeStmt;

struct NodeScope {
    std::vector<NodeStmt *> stmts;

    new_falc(NodeScope,);
};

struct NodeStmtIf {
    NodeExpr *expr;
    NodeStmt *stmt;

    new_falc(NodeStmtIf, x->expr = expr; x->stmt = stmt, NodeExpr *expr, NodeStmt *stmt);
    new_falc(NodeStmtIf, x->expr = expr; x->stmt = nullptr, NodeExpr *expr);
    new_falc(NodeStmtIf, x->expr = nullptr; x->stmt = nullptr);
};

struct NodeStmt {
    std::variant<NodeStmtExit*, NodeStmtLet*, NodeScope*, NodeStmtIf*> var;

    new_falc(NodeStmt, x->var = var, NodeStmtExit *var);
    new_falc(NodeStmt, x->var = var, NodeStmtLet *var);
    new_falc(NodeStmt, x->var = var, NodeScope *var);
    new_falc(NodeStmt, x->var = var, NodeStmtIf *var);
};

struct NodeProg {
    std::vector<NodeStmt *> stmts;
};
#define check_consume(ttype, msg) if (peekc().type == ttype ) { consume(); } else {std::cerr << (msg) << std::endl;exit(1);}
class Parser {
public:
    explicit Parser(std::vector<Token> tokens) : m_tokens(std::move(tokens)), m_allocator(1024 * 1024 * 4) {} // 4 MB

    std::optional<NodeExprTerm *> parse_term() {
        if (peekc().type == TokenType::l_int) {
            auto term_int_lit = aalloc(NodeExprTermIntLit, consume());
            return aalloc(NodeExprTerm, term_int_lit);
        } else if (peekc().type == TokenType::ident) {
            auto term_ident = aalloc(NodeExprTermIdent, consume());
            return aalloc(NodeExprTerm, term_ident);
        } else if (peekc().type == TokenType::c_lparen) {
            consume();
            auto expr = parse_expr();
            if (!expr.has_value()) {
                std::cerr << "Invalid expression in parenthesis" << std::endl;
                std::cerr << "got token type " << (int) peekval().type << std::endl;
                exit(1);
            }
            check_consume(TokenType::c_rparen, "Expected ')' after expression in parenthesis");
            auto term_paren = aalloc(NodeExprTermParen, expr.value());
            return aalloc(NodeExprTerm, term_paren);
        } else {
            return std::nullopt;
        }
    }

    std::optional<NodeExpr *> parse_expr(int min_prec = 0) {
        auto term_lhs = parse_term();
        if (!term_lhs.has_value()) {
            std::cerr << "Invalid expression" << std::endl;
            std::cerr << "got token type " << (int) peekval().type << std::endl;
            exit(1);
        }
        auto lexpr = aalloc(NodeExpr, term_lhs.value());
        // use precedence climbing
        while (true) {
            auto next = peek();
            std::optional<int> prec;
            if (next.has_value()) {
                prec = bin_prec(next.value().type);
                if (!prec.has_value() || prec.value() < min_prec) {
                    break;
                }
            } else {
                break;
            }

            int next_min_prec = prec.value() + 1;
            auto op = consume();
            auto term_rhs = parse_expr(next_min_prec);
            if (!term_rhs.has_value()) {
                std::cerr << "Invalid expressiona" << std::endl;
                std::cerr << "OP" << (int) op.type << std::endl;
                std::cerr << "got token type " << (int) peekval().type << std::endl;
                exit(1);
            }
            auto expr =  aalloc(NodeExprBin);
            auto lhs2 = aalloc(NodeExpr, expr);
#define binop(tok, typ) if (op.type == tok) { lhs2->var = lexpr->var; auto mbinex = aalloc(typ, lhs2, term_rhs.value()); expr->var = mbinex; }
            binop(TokenType::c_plus, NodeExprBinAdd)
            else binop(TokenType::c_minus, NodeExprBinSub)
            else binop(TokenType::c_star, NodeExprBinMul)
            else binop(TokenType::c_slash, NodeExprBinDiv)
            else binop(TokenType::c_mod, NodeExprBinMod)
#undef binop
            else {
                std::cerr << "Invalid expressionb" << std::endl;
                std::cerr << "got token type " << (int) peekval().type << std::endl;
                exit(1);
            }
            lexpr->var = expr;
        }
        return lexpr;
    }

    std::optional<NodeScope*> parse_scope() {
        if (!(peekc().type == TokenType::c_lbrace)) {
            return {};
        }
        consume();
        auto scope = aalloc(NodeScope);
        while (auto stmt = parse_stmt()) {
            scope->stmts.push_back(stmt.value());
        }
        check_consume(TokenType::c_rbrace, "Expected '}' after scope");
        return scope;

    }

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
        } else if (peekc().type == TokenType::c_lbrace) {
            if (auto scope = parse_scope()) {
                return aalloc(NodeStmt, scope.value());
            } else {
                std::cerr << "Invalid scope!" << std::endl;
                exit(1);
            }

        } else if (peekc().type == TokenType::k_if) {
            consume();
            check_consume(TokenType::c_lparen, "Expected '(' after if");
            auto stmt_if = aalloc(NodeStmtIf);
            if (auto expr = parse_expr()) {
                stmt_if->expr = expr.value();
            } else {
                std::cerr << "Expected expr in if" << std::endl;
                exit(1);
            }
            check_consume(TokenType::c_rparen, "Expected ')' after if");
            if (auto stmt = parse_stmt()) {
                stmt_if->stmt = stmt.value();
            } else {
                std::cerr << "Expected scope" << std::endl;
                exit(1);
            }
            return aalloc(NodeStmt, stmt_if);

        }
        else {
            return {};
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
#undef check_consume
#undef new_falc
#undef aalloc
#undef peekc
#undef peekval
#undef peekhas
