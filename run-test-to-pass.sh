#!/bin/bash

BIN="./sesterl"
TARGET_DIR="test/_generated/"

compile_erlang_code() {
  for TARGET in *.erl; do
      erlc "$TARGET"
  done
}

for SOURCE in test/pass/*.sest; do
    "$BIN" "$SOURCE" -o "$TARGET_DIR"
done

cd "$TARGET_DIR" && compile_erlang_code
