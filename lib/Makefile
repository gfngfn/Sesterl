
SESTERL=../sesterl
TARGET_DIR=_generated

all:
	mkdir -p $(TARGET_DIR)
	$(SESTERL) sample_project.sest -o $(TARGET_DIR) --verbose
	./compile_targets.sh
