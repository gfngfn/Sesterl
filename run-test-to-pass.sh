#!/bin/bash

BIN="./sesterl"
TARGET_DIR="test/_generated/"

compile_erlang_code() {
    for TARGET in *.erl; do
        echo "Compiling '$TARGET' by erlc ..."
        erlc "$TARGET"
    done
}

for SOURCE in test/pass/*.sest; do
    echo "Compiling '$SOURCE' by sesterl ..."
    "$BIN" "$SOURCE" -o "$TARGET_DIR"
done

cd "$TARGET_DIR" && compile_erlang_code
