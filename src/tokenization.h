#pragma once

#include <string>
#include <utility>
#include <vector>
#include <iostream>
#include <optional>

#define peekhas(i) peek(i).has_value()
#define peekval(i) peek(i).value()


#define tok_c(ch, tok) if (peek().value() == ch) {consume();tokens.emplace_back(tok);}
#define tok_k(str, tok) if (buf == str) {tokens.emplace_back(tok);}

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
    c_plus,
    c_minus,
    c_star,
    c_slash,
    c_mod
};

std::optional<int> bin_prec(TokenType type) {
    if (type == TokenType::c_plus || type == TokenType::c_minus) {
        return 0;
    } else if (type == TokenType::c_star || type == TokenType::c_slash || type == TokenType::c_mod) {
        return 1;
    } else {
        return std::nullopt;
    }
}

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

        while (peekhas()) {
            if (std::isalpha(peekval()) || peekval() == '_') {
                buf.push_back(consume());
                while (peekhas() && (std::isalnum(peekval()) || peekval() == '_')) {
                    buf.push_back(consume());
                }
                tok_k("exit", TokenType::b_exit)
                else tok_k("if", TokenType::k_if)
                else tok_k("else", TokenType::k_else)
                else tok_k("while", TokenType::k_while)
                else tok_k("for", TokenType::k_for)
                else tok_k("break", TokenType::k_break)
                else tok_k("continue", TokenType::k_continue)
                else tok_k("let", TokenType::k_let)
                else tok_k("return", TokenType::k_return)
                else {
                    tokens.emplace_back(TokenType::ident, buf);
                }
                buf.clear();
            } else if (std::isdigit(peekval())) {
                buf.push_back(consume());
                while (peekhas() && std::isdigit(peekval())) {
                    buf.push_back(consume());
                }
                if (peekhas() && peekval() == '.') {
                    buf.push_back(consume());
                    while (peekhas() && std::isdigit(peekval())) {
                        buf.push_back(consume());
                    }
                    tokens.emplace_back(TokenType::l_float, buf);
                } else {
                    tokens.emplace_back(TokenType::l_int, buf);
                }
                buf.clear();
            } else tok_c(';', TokenType::c_semi)
            else tok_c('(', TokenType::c_lparen)
            else tok_c(')', TokenType::c_rparen)
            else tok_c('=', TokenType::c_eq)
            else tok_c('+', TokenType::c_plus)
            else tok_c('-', TokenType::c_minus)
            else tok_c('*', TokenType::c_star)
            else tok_c('/', TokenType::c_slash)
            else tok_c('%', TokenType::c_mod)
            else if (std::isspace(peekval())) {
                consume();
            } else {
                std::cerr << "Unrecognized character: '" << peekval() << "'" << std::endl;
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