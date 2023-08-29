#pragma once

#include <utility>
#include <variant>
#include "tokenization.h"

#define peekhas(i) peek(i).has_value()
#define peekval(i) peek(i).value()
#define peekc(i) peek(i).has_value() && peek(i).value()

struct NodeExprIntLit {
    Token int_lit;
};

struct NodeExprIdent {
    Token ident;
};

struct NodeExpr {
    std::variant<NodeExprIntLit, NodeExprIdent> var = NodeExprIntLit{.int_lit = Token(TokenType::err)};
    static NodeExpr newIntLit(Token tok) {
        return NodeExpr{.var = NodeExprIntLit{.int_lit = std::move(tok)}};
    }
    static NodeExpr newIdent(Token tok) {
        return NodeExpr{.var = NodeExprIdent{.ident = std::move(tok)}};
    }
};

struct NodeStmtExit {

    NodeExpr expr;
    explicit NodeStmtExit(NodeExpr expr) : expr(std::move(expr)) {}
    NodeStmtExit() = default;

};

struct NodeStmtLet {
    Token ident;
    NodeExpr expr;
    NodeStmtLet(Token ident, NodeExpr expr) : ident(std::move(ident)), expr(std::move(expr)) {}
    NodeStmtLet() = default;
};

struct NodeStmt {
    std::variant<NodeStmtExit, NodeStmtLet> var;
    explicit NodeStmt(NodeStmtExit stmt_exit) : var(std::move(stmt_exit)) {}
    explicit NodeStmt(NodeStmtLet stmt_let) : var(std::move(stmt_let)) {}
    NodeStmt() = default;
};

struct NodeProg {
    std::vector<NodeStmt> stmts;
};

class Parser {
public:
    explicit Parser(std::vector<Token> tokens): m_tokens(std::move(tokens)) {}

    std::optional<NodeExpr> parse_expr() {
        if (peekc().type == TokenType::l_int) {
            return NodeExpr::newIntLit(consume());
        } else if (peekc().type == TokenType::ident) {
            return NodeExpr::newIdent(consume());
        }
        return std::nullopt;
    }

    std::optional<NodeStmt> parse_stmt() {
        if (peekc().type == TokenType::b_exit && peekc(1).type == TokenType::c_lparen) {
            consume(); consume();
            NodeStmtExit stmt_exit;
            if (auto node_expr = parse_expr()) {
                stmt_exit = NodeStmtExit(node_expr.value());
            } else {
                std::cerr << "Invalid expression in exit" << std::endl;
                std::cerr << "got token type " << (int)peekval().type << std::endl;
                exit(1);
            }
            if (peekc().type == TokenType::c_rparen) {
                consume();
            } else {
                std::cerr << "Expected ')' after exit" << std::endl;
                exit(1);
            }
            if (peekc().type == TokenType::c_semi) {
                consume();
            } else {
                std::cerr << "Expected ';' after exit" << std::endl;
                exit(1);
            }
            return NodeStmt(stmt_exit);
        } else if (peekc().type == TokenType::k_let && peekc(1).type == TokenType::ident && peekc(2).type == TokenType::c_eq) {
            consume();
            Token ident = consume();
            consume();
            NodeStmtLet stmt_let;
            if (auto node_expr = parse_expr()) {
                stmt_let = NodeStmtLet(ident, node_expr.value());
            } else {
                std::cerr << "Invalid expression in let" << std::endl;
                std::cerr << "got token type " << (int)peekval().type << std::endl;
                exit(1);
            }
            if (peekc().type == TokenType::c_semi) {
                consume();
            } else {
                std::cerr << "Expected ';' after let" << std::endl;
                exit(1);
            }
            return NodeStmt(stmt_let);
        } else {
            std::cerr << "Invalid statement" << std::endl;
            std::cerr << "got token type " << (int)peekval().type << std::endl;
            exit(1);
        }
    }

    std::optional<NodeProg> parse_prog() {
        NodeProg prog;
        while (peekhas()) {
            if (auto stmt = parse_stmt()) {
                prog.stmts.push_back(stmt.value());

            } else {
                std::cerr << "Invalid statement" << std::endl;
                std::cerr << "got token type " << (int)peekval().type << std::endl;
                exit(1);
            }
        }
        return prog;
    }

private:
    [[nodiscard]] std::optional<Token> peek(int off=0) const {
        if (m_index + off >= m_tokens.size()) {
            return std::nullopt;
        }
        return m_tokens.at(m_index+off);
    }

    Token consume() {
        return m_tokens.at(m_index++);
    }

    const std::vector<Token> m_tokens;
    size_t m_index = 0;
};