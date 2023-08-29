#pragma once

#include <string>
#include <utility>
#include <vector>
#include <iostream>
#include <optional>

enum class TokenType { // k_ = keyword l_ = literal c_ = char/symbol b_ = builtin
    err, // 0

    b_exit, // 1

    k_return, // 2
    k_if,
    k_else,
    k_while,
    k_for,
    k_break,
    k_continue,
    k_let,

    l_int, // 10
    l_float,
    l_string,
    l_char,
    l_bool,
    l_null,
    ident,

    c_semi, // 17
    c_lparen,
    c_rparen,
    c_eq,
};

struct Token {
    Token(TokenType type, std::string value) : type(type), value(value) {}
    explicit Token(TokenType type) : type(type) {}
    Token() = default;


    TokenType type=TokenType::err;
    std::optional<std::string> value{};
};

class Tokenizer {
public:
    explicit Tokenizer(std::string src) : m_src(std::move(src)) {}

    std::vector<Token> tokenize() {
        std::string buf;
        std::vector<Token> tokens;

        while (peek().has_value()) {
            if (std::isalpha(peek().value()) || peek().value() == '_') {
                buf.push_back(consume());
                while (peek().has_value() && (std::isalnum(peek().value()) || peek().value() == '_')) {
                    buf.push_back(consume());
                }
                if (buf == "exit") {
                    tokens.emplace_back(TokenType::b_exit);
                } else if (buf == "if") {
                    tokens.emplace_back(TokenType::k_if);
                } else if (buf == "else") {
                    tokens.emplace_back(TokenType::k_else);
                } else if (buf == "while") {
                    tokens.emplace_back(TokenType::k_while);
                } else if (buf == "for") {
                    tokens.emplace_back(TokenType::k_for);
                } else if (buf == "break") {
                    tokens.emplace_back(TokenType::k_break);
                } else if (buf == "continue") {
                    tokens.emplace_back(TokenType::k_continue);
                } else if (buf == "let") {
                    tokens.emplace_back(TokenType::k_let);
                } else {
                    tokens.emplace_back(TokenType::ident, buf);
                }
                buf.clear();
            } else if (std::isdigit(peek().value())) {
                buf.push_back(consume());
                while (peek().has_value() && std::isdigit(peek().value())) { // TODO: test floats
                    buf.push_back(consume());
                }
                if (peek().has_value() && peek().value() == '.') {
                    buf.push_back(consume());
                    while (peek().has_value() && std::isdigit(peek().value())) {
                        buf.push_back(consume());
                    }
                    tokens.emplace_back(TokenType::l_float, buf);
                } else {
                    tokens.emplace_back(TokenType::l_int, buf);
                }
                buf.clear();
            } else if (peek().value() == ';') {
                consume();
                tokens.emplace_back(TokenType::c_semi);
            } else if (peek().value() == '(') {
                consume();
                tokens.emplace_back(TokenType::c_lparen);
            } else if (peek().value() == ')') {
                consume();
                tokens.emplace_back(TokenType::c_rparen);
            }else if (peek().value() == '=') {
                consume();
                tokens.emplace_back(TokenType::c_eq);
            } else if (std::isspace(peek().value())) {
                consume();
            } else {
                std::cerr << "Unrecognized character: '" << peek().value() << "'" << std::endl;
                exit(-1);
            }
        }
        m_index = 0;
        return tokens;
    }

private:
    [[nodiscard]] std::optional<char> peek(int off=0) const {
        if (m_index + off >= m_src.length()) {
            return std::nullopt;
        }
        return m_src.at(m_index+off);
    }

    char consume() {
        return m_src.at(m_index++);
    }

    const std::string m_src;
    size_t m_index = 0;

};