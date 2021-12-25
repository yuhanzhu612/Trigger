# Run

```shell
cd libraries/NEMU
make menuconfig
```

Base ISA (riscv64)
Testing and Debugging  ---> Enable differential testing (NEW)
FPU Emulation (Use softfloat library)
Detecting misaligned memory accessing (By software emulation)
Processor difftest reference config  ---> Build shared library as processor difftest reference

```shell
make
vim libraries/difftest/config/verilator.mk 
```

-# WITH_DRAMSIM3 ?= 1
+WITH_DRAMSIM3 ?= 1

```shell
vim libraries/difftest/src/test/csrc/common/axi4.h
```

-memcpy(dest, src, sizeof(uint64_t)*AXI_DATA_WIDTH_64);
+memcpy(&dest, &src, sizeof(uint64_t)*AXI_DATA_WIDTH_64);

```shell
./build.sh -e chisel_cpu_diff -d -s -a "-i inst_diff.bin" -m "EMU_TRACE=1" -b
```