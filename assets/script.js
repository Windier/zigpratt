let wasmModule = null;
let wasmMemory = null;

// Load WebAssembly module
async function loadWasm() {
    try {
        const response = await fetch('./parser.wasm');
        const bytes = await response.arrayBuffer();
        const wasmInstance = await WebAssembly.instantiate(bytes, {});
        
        wasmModule = wasmInstance.instance.exports;
        wasmMemory = wasmModule.memory;
        
        // Debug: Log available exports
        console.log('Available WASM exports:', Object.keys(wasmModule));
        console.log('malloc available:', typeof wasmModule.malloc);
        console.log('parseExpression available:', typeof wasmModule.parseExpression);
        console.log('parseExpressionToTree available:', typeof wasmModule.parseExpressionToTree);
        console.log('getOutputPtr available:', typeof wasmModule.getOutputPtr);
        console.log('getOutputLen available:', typeof wasmModule.getOutputLen);
        
        // Update status
        document.getElementById('status').textContent = 'wasm module loaded';
        document.getElementById('status').className = 'status ready';
        
        // Enable controls
        document.getElementById('expression').disabled = false;
        document.getElementById('outputFormat').disabled = false;
        document.getElementById('parseBtn').disabled = false;
        
        // Focus on input
        document.getElementById('expression').focus();
        
    } catch (error) {
        console.error('Failed to load WASM module:', error);
        document.getElementById('status').textContent = 'failed to load wasm module: ' + error.message;
        document.getElementById('status').className = 'status error-wasm';
    }
}

// Function to format the JSON tree by only showing types in a clean tree structure
function formatTreeDisplay(node, depth = 0) {
    const indent = "  ".repeat(depth);
    
    if (typeof node === 'string') {
        return node;
    }
    
    if (!node || typeof node !== 'object') {
        return String(node);
    }
    
    let result = indent + node.type;
    
    if (node.children && Array.isArray(node.children)) {
        result += "\n";
        for (let i = 0; i < node.children.length; i++) {
            result += formatTreeDisplay(node.children[i], depth + 1);
            if (i < node.children.length - 1) {
                result += "\n";
            }
        }
    } else if (node.text) {
        result += ": " + node.text;
    } else if (node.value !== undefined) {
        result += ": " + node.value;
    }
    
    return result;
}

function parseExpression() {
    const expressionInput = document.getElementById('expression');
    const outputFormatSelect = document.getElementById('outputFormat');
    const resultDiv = document.getElementById('result');
    const parseBtn = document.getElementById('parseBtn');
    
    const expression = expressionInput.value.trim();
    const outputFormat = outputFormatSelect.value;
    
    if (!expression) {
        resultDiv.textContent = 'Please enter an expression';
        resultDiv.className = 'result error';
        return;
    }
    
    if (!wasmModule) {
        resultDiv.textContent = 'WebAssembly module not loaded';
        resultDiv.className = 'result error';
        return;
    }
    
    try {
        // Show loading state
        resultDiv.textContent = 'Parsing...';
        resultDiv.className = 'result loading';
        parseBtn.disabled = true;
        
        // Convert string to bytes
        const encoder = new TextEncoder();
        const inputBytes = encoder.encode(expression);
        
        // Allocate memory in WASM
        const inputPtr = wasmModule.malloc ? wasmModule.malloc(inputBytes.length) : 0;
        
        // Copy input to WASM memory (fallback if malloc is not available)
        const memoryView = new Uint8Array(wasmMemory.buffer);
        const startPtr = inputPtr || 1024; // Use a fixed offset if malloc is not available
        memoryView.set(inputBytes, startPtr);
        
        // Call the appropriate parser function
        let success;
        if (outputFormat === 'tree') {
            success = wasmModule.parseExpressionToTree(startPtr, inputBytes.length);
        } else {
            success = wasmModule.parseExpression(startPtr, inputBytes.length);
        }
        
        if (success) {
            // Get result
            const outputPtr = wasmModule.getOutputPtr();
            const outputLen = wasmModule.getOutputLen();
            
            const outputBytes = new Uint8Array(wasmMemory.buffer, outputPtr, outputLen);
            const decoder = new TextDecoder();
            const result = decoder.decode(outputBytes);
            
            // Format the result based on output type
            if (outputFormat === 'tree') {
                try {
                    const jsonObj = JSON.parse(result);
                    resultDiv.textContent = formatTreeDisplay(jsonObj);
                } catch (e) {
                    resultDiv.textContent = result; // Fallback to raw text
                }
            } else {
                resultDiv.textContent = result;
            }
            resultDiv.className = 'result success';
        } else {
            // Get error message
            const outputPtr = wasmModule.getOutputPtr();
            const outputLen = wasmModule.getOutputLen();
            
            const outputBytes = new Uint8Array(wasmMemory.buffer, outputPtr, outputLen);
            const decoder = new TextDecoder();
            const errorMsg = decoder.decode(outputBytes);
            
            resultDiv.textContent = errorMsg;
            resultDiv.className = 'result error';
        }
        
        // Clean up memory if malloc was used
        if (wasmModule.free && inputPtr) {
            wasmModule.free(inputPtr);
        }
        
    } catch (error) {
        console.error('Error parsing expression:', error);
        resultDiv.textContent = 'Error: ' + error.message;
        resultDiv.className = 'result error';
    } finally {
        parseBtn.disabled = false;
    }
}

function tryExample(expression) {
    document.getElementById('expression').value = expression;
    parseExpression();
}

// Handle Enter key
document.addEventListener('DOMContentLoaded', function() {
    document.getElementById('expression').addEventListener('keypress', function(event) {
        if (event.key === 'Enter') {
            parseExpression();
        }
    });
});

// Load WASM on page load
loadWasm();
