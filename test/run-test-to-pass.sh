#!/bin/bash

TARGET_DIR="test/_generated/"

for SOURCE in test/pass/*.sest; do
    ./sesterl "$SOURCE" -o "$TARGET_DIR"
done

for TARGET in "$TARGET_DIR"/*.erl; do
    erlc "$TARGET"
done
