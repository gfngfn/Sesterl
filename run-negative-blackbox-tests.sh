#!/bin/bash

BIN="./sesterl"
SOURCE_DIR="test/fail"
TARGET_DIR="test/_generated"

mkdir -p "$TARGET_DIR"

NO_ERRORS=()

for PKG_DIR in "$SOURCE_DIR"/*/; do
    echo "Compiling package '$PKG_DIR' ..."
    "$BIN" "$PKG_DIR" -o "$TARGET_DIR"
    STATUS=$?
    if [ $STATUS -eq 0 ]; then
        NO_ERRORS+=("$PKG_DIR")
    fi
done

for SOURCE in "$SOURCE_DIR"/*.sest; do
    echo "Compiling standalone file '$SOURCE' ..."
    "$BIN" "$SOURCE" -o "$TARGET_DIR"
    STATUS=$?
    if [ $STATUS -eq 0 ]; then
        NO_ERRORS+=("$SOURCE")
    fi
done

RET=0
for X in "${NO_ERRORS[@]}"; do
    RET=1
    echo "[FAIL] $X"
done
if [ $RET -eq 0 ]; then
    echo "All tests have passed."
fi

exit $RET
