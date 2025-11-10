# Zig Pratt Parser

main source of knowledge: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

### Run locally
```bash
zig build run -- --help
```

### Run tests
```bash
zig build test
```

### Build WASM and run web interface
```bash
zig build wasm
cd assets
cp ../zig-out/bin/parser.wasm .
python3 -m http.server 8081
```
Then open `http://localhost:8081`
