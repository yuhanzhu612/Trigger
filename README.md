# Preparation for the test

### SSH transfer
```shell
git clone --recursive -b super_scalar git@github.com:yuhanzhu612/Trigger.git

```

If the submodule repository clone fails, you will try

```shell
git submodule update --init --recursive

```

# Run

```shell
cd libraries/NEMU
make menuconfig
make riscv64-xs-ref_defconfig
make -j
```

```shell
cd ../..
vim libraries/difftest/config/verilator.mk 

-# WITH_DRAMSIM3 ?= 1
+WITH_DRAMSIM3 ?= 1
```

```shell
vim libraries/difftest/src/test/csrc/common/axi4.h

-memcpy(dest, src, sizeof(uint64_t)*AXI_DATA_WIDTH_64);
+memcpy(&dest, &src, sizeof(uint64_t)*AXI_DATA_WIDTH_64);
```

```shell
./build.sh -e chisel_cpu_diff -d -s -a "-i inst_diff.bin" -m "EMU_TRACE=1" -b
```
