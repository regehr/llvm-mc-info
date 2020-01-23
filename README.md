# Usage

```
mkdir build && cd build
cmake .. -G Ninja
ninja
```

# TODO

- optimize the IR before compiling to machine code

- see how fast this all is

- get cycles from MCA

- make souper2llvm respect values with external uses

- support multiple architectures, average across them or something
  - x64
  - ARM64
  - x86?
  - ARM32?
  - mips or ppc or RISC-V or something?
