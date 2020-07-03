#!/bin/bash

BIN="./sesterl"
SOURCE_DIR="test/pass"
TARGET_DIR="test/_generated"


for SOURCE in "$SOURCE_DIR"/*.sest; do
    echo "Compiling '$SOURCE' by sesterl ..."
    "$BIN" "$SOURCE" -o "$TARGET_DIR"
done

for TARGET in "$TARGET_DIR"/*.erl; do
    echo "Compiling '$TARGET' by erlc ..."
    erlc -o "$TARGET_DIR" "$TARGET"
done
