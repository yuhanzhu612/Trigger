# Trigger

My cpu!


### cpu_axi_diff

`projects/cpu_diff`目录下存放了通过`AXI总线`接入`香山difftest框架`的`verilog`版本单周期`RISC-V`CPU例程源码，源码实现了`RV64I`指令`addi`和`AXI总线`读逻辑。可以使用下面的命令编译和仿真。

```shell
./build.sh -e cpu_axi_diff -d -s -a "-i inst_diff.bin --wave-path=wave.vcd --dump-wave -b 0" -m "EMU_TRACE=1 WITH_DRAMSIM3=1" -b
```

### chisel_cpu_diff

`projects/cpu_diff`目录下存放了接入`香山difftest框架`的`chisel`版本单周期`RISC-V` CPU例程源码，源码实现了`RV64I`指令`addi`。可以使用下面的命令编译和仿真。

```shell
./build.sh -e chisel_cpu_diff -d -s -a "-i inst_diff.bin" -m "EMU_TRACE=1" -b
```