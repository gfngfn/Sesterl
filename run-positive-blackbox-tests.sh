#!/bin/bash

command -v gsed
STATUS=$?
if [ $STATUS -eq 0 ]; then
    GNU_SED="gsed"
else
    command -v sed
    STATUS=$?
    if [ $STATUS -eq 0 ]; then
        GNU_SED="sed"
    else
        echo "GNU sed is not installed. Stop."
    fi
fi

BIN="./sesterl"
SOURCE_DIR="test/pass"
TARGET_DIR="test/_generated"

mkdir -p "$TARGET_DIR"

ERRORS=()

for PKG_DIR in "$SOURCE_DIR"/*/; do
    echo "Compiling package '$PKG_DIR' ..."
    "$BIN" build "$PKG_DIR" -o "$TARGET_DIR"
    STATUS=$?
    if [ $STATUS -ne 0 ]; then
        ERRORS+=("$PKG_DIR")
    fi
done

for SOURCE in "$SOURCE_DIR"/*.sest; do
    echo "Compiling standalone file '$SOURCE' by sesterl ..."
    "$BIN" build "$SOURCE" -o "$TARGET_DIR"
    STATUS=$?
    if [ $STATUS -ne 0 ]; then
        ERRORS+=("$SOURCE")
    fi
done

for TARGET in "$TARGET_DIR"/*.erl; do
    echo "Compiling '$TARGET' by erlc ..."
    erlc -o "$TARGET_DIR" "$TARGET"
    STATUS=$?
    if [ $STATUS -ne 0 ]; then
        ERRORS+=("$TARGET")
    fi
done

CURDIR=$(pwd)
cd "$TARGET_DIR" || exit
for TARGET in *.erl; do
    NUM="$(grep -c "'main'/1" "$TARGET")"
    if [ "$NUM" -eq 0 ]; then
        echo "Skip '$TARGET' due to the absence of main/1."
    else
        echo "Executing '$TARGET' by escript ..."
        $GNU_SED '1s|^|#!/usr/local/bin/escript\n|' -i "$TARGET"
        escript "$TARGET"
        STATUS=$?
        if [ $STATUS -ne 0 ]; then
            ERRORS+=("$TARGET")
        fi
    fi
done
cd "$CURDIR" || exit

RET=0
for X in "${ERRORS[@]}"; do
    RET=1
    echo "[FAIL] $X"
done
if [ $RET -eq 0 ]; then
    echo "All tests have passed."
fi

exit $RET
