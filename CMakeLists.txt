cmake_minimum_required(VERSION 3.22)
project(ludumc)

set(CMAKE_CXX_STANDARD 20)

add_executable(ludumc src/main.cpp
        src/tokenization.h
        src/parser.h
        src/generation.h)
