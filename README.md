# Trigger

Trigger是一个使用chisel写的4级流水线顺序单发射处理器核，兼容RVI64，可成功起rt-thread.
代码存放在[这里](https://github.com/yuhanzhu612/Trigger/tree/super_scalar/projects/chisel_cpu_diff/src/main/scala)

developer：yhz


## 流水线设计

### 取指级

主要任务是取指令。将分支预测器预测出来的nextpc送给Icache，Icache将该nextpc所在块的指令返回给取值级。只有收到了有效指令，有效数据才会流向下一级.

### 译码级

主要任务为译码、取源操作数和判断下一条指令是否转移。译码级收到取值级送过来的有效指令，将其译码，分析该指令的行为。如果该条指令会引起下一条指令的转移，则会修改分支预测器.

### 执行级

主要任务为产生计算结果，如果是load/store指令，则会在该级访Dcache，直到访问结束。执行级收到译码级送过来的源操作数，送到功能单元得到计算结果，送到写回级写回.

### 写回级

主要任务是写寄存器堆和写CSR.

