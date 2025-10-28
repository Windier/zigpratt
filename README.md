# Zig Pratt Parser

main source of knowledge: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

### Run locally
```bash
zig run parser.zig
```

### Run tests
```bash
zig test parser.zig
```

### Build WASM and run web interface
```bash
zig build wasm
cp zig-out/bin/parser.wasm .
python -m http.server 8081
```
Then open `http://localhost:8081`

## Files

- `parser.zig` - Main parser with stdout
- `wasm.zig` - Same as the previous one, but this is the WASM version with JSON output
- `index.html`, `style.css`, `script.js` - Web interface