
SESTERL=../sesterl
TARGET_DIR=_generated

all:
	mkdir -p $(TARGET_DIR)
	$(SESTERL) stdlib/ -o $(TARGET_DIR) --verbose
	./compile_targets.sh

clean:
	rm $(TARGET_DIR)/*
