
TARGET_DIR=_generated

for TARGET in "$TARGET_DIR"/*.erl; do
    echo "Compiling '$TARGET' by erlc ..."
    erlc -o "$TARGET_DIR" "$TARGET"
done
