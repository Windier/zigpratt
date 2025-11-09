# Zig Pratt Parser

main source of knowledge: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

### Run locally
```bash
zig run src/parser.zig
```

### Run tests
```bash
zig test src/parser.zig
```

### Build WASM and run web interface
```bash
zig build wasm
cd assets
cp ../zig-out/bin/parser.wasm .

```
Then open `http://localhost:8081`

## Files

- `parser.zig` - Main parser with stdout
- `wasm.zig` - Same as the previous one, but this is the WASM version with JSON output
- `index.html`, `style.css`, `script.js` - Web interface