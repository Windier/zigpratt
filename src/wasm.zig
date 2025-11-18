const std = @import("std");
const ArrayList = std.ArrayList;

const tex = @import("root.zig");

pub const std_options: std.Options = .{
    .logFn = logFn,
};

pub fn logFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = level;
    _ = scope;
    _ = format;
    _ = args;
    // No-op for WASM
}

// WASM memory for string exchange
var output_writer: [2048]u8 = undefined;
var output_len: usize = 0;

// Simple test function to verify WASM loading
export fn testFunction() i32 {
    return 42;
}

// Simple memory allocator for WASM
var memory_pool: [4096]u8 = undefined;
var memory_offset: usize = 0;

// Export function to allocate memory
export fn malloc(size: usize) usize {
    if (memory_offset + size > memory_pool.len) {
        return 0; // Return 0 if out of memory
    }
    const offset = memory_offset;
    memory_offset += size;
    return @intFromPtr(&memory_pool[offset]);
}

export fn free(ptr: usize) void {
    _ = ptr;
}

// Export function to get the output writer pointer
export fn getOutputPtr() [*]u8 {
    return &output_writer;
}

// Export function to get the output length
export fn getOutputLen() usize {
    return output_len;
}

// Simplified polishToString for WASM
fn wasmPolishToString(writer: *std.Io.Writer, expr: *const tex.Expression, source: []const u8) !void {
    switch (expr.type) {
        .Variable => {
            try writer.print(" {s}", .{source[expr.pos.from..expr.pos.to]});
        },
        .FunctionName => {
            try writer.print(" func", .{});
        },
        .Number => {
            if (expr.value) |val| {
                switch (val) {
                    .i => |i| try writer.print(" {d}", .{i}),
                    .f => |f| try writer.print(" {d}", .{f}),
                    else => try writer.print(" ?", .{}),
                }
            } else {
                try writer.print("? ", .{});
            }
        },
        .Add => {
            try writer.print(" (+", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Sub => {
            try writer.print(" (-", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Mul => {
            try writer.print(" (*", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .iMul => {
            try writer.print(" (*i", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Div => {
            try writer.print(" (/", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Pow => {
            try writer.print(" (^", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Dot => {
            try writer.print(" (.", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Assignment => {
            try writer.print(" (=", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .With => {
            try writer.print(" (with ", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .UnaryMinus => {
            try writer.print(" (-u", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
            }
            try writer.print(")", .{});
        },
        .UnaryPlus => {
            try writer.print(" (+", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
            }
            try writer.print(")", .{});
        },
        .Object => {
            try writer.print(" (obj", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try wasmPolishToString(writer, &children[i], source);
                }
            }
            try writer.print(")", .{});
        },
        .Arguments => {
            try writer.print(" (args", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try wasmPolishToString(writer, &children[i], source);
                }
            }
            try writer.print(")", .{});
        },
        .Paren => {
            try writer.print(" (paren", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try wasmPolishToString(writer, &children[i], source);
                }
            }
            try writer.print(")", .{});
        },
        .Comma => {
            try writer.print(", ", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
        },
        .FunctionCall => {
            try writer.print(" (call", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Juxt => {
            try writer.print(" (juxt", .{});
            if (expr.children) |children| {
                try wasmPolishToString(writer, &children[0], source);
                try wasmPolishToString(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Invalid => {
            try writer.print("INVALID ", .{});
        },
    }
}

// JSON tree serialization for WASM
fn wasmTreeToJson(expr: *const tex.Expression, source: []const u8, writer: *std.Io.Writer) !void {
    try writer.print("{{\"type\":\"{s}\"", .{@tagName(expr.type)});

    // Add position info
    try writer.print(",\"pos\":{{\"from\":{d},\"to\":{d}}}", .{ expr.pos.from, expr.pos.to });

    // Add value if present
    if (expr.value) |val| {
        switch (val) {
            .i => |i| try writer.print(",\"value\":{d}", .{i}),
            .f => |f| try writer.print(",\"value\":{d}", .{f}),
            .length => |len| try writer.print(",\"length\":{d}", .{len}),
        }
    }

    // Add text for variables/operators/functions
    if (expr.type == .Variable or expr.type == .FunctionName) {
        try writer.print(",\"text\":\"{s}\"", .{source[expr.pos.from..expr.pos.to]});
    }

    // Add children recursively
    if (expr.children) |children| {
        try writer.print(",\"children\":[", .{});
        const length = switch (expr.type) {
            .Object, .Arguments, .Paren => expr.value.?.length,
            .UnaryMinus, .UnaryPlus => 1,
            else => 2, // Binary operations
        };

        for (0..length) |i| {
            if (i > 0) try writer.print(",", .{});
            try wasmTreeToJson(&children[i], source, writer);
        }
        try writer.print("]", .{});
    }

    try writer.print("}}", .{});
}

export fn parseExpression(input_ptr: [*]const u8, input_len: usize) bool {
    // Reset output
    output_len = 0;

    // Create a null-terminated string from the input
    if (input_len >= output_writer.len - 1) {
        // Input too long, copy error message
        const error_msg = "Error: Input too long";
        @memcpy(output_writer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    }

    // Copy input and null-terminate
    var input_writer: [2048]u8 = undefined;
    @memcpy(input_writer[0..input_len], input_ptr[0..input_len]);
    input_writer[input_len] = 0;
    const expr: [:0]const u8 = input_writer[0..input_len :0];

    // Create arena allocator
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse the expression
    const result = parseExpressionInternal(expr, allocator) catch |err| {
        const error_msg = switch (err) {
            error.UnexpectedToken => "Error: Unexpected token",
            error.ExpectedSomething => "Error: Expected something",
            error.UnmatchedParentheses => "Error: Unmatched parentheses",
            error.EmptyParentheses => "Error: Empty parentheses",
            error.InvalidOperator => "Error: Invalid operator",
            error.OutOfMemory => "Error: Out of memory",
            error.Overflow => "Error: Overflow",
            error.InvalidCharacter => "Error: Invalid character",
            error.WriteFailed => "Error: Write failed",
        };
        @memcpy(output_writer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    };

    // Copy result to output writer
    if (result.len >= output_writer.len) {
        const error_msg = "Error: Result too long";
        @memcpy(output_writer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    }

    @memcpy(output_writer[0..result.len], result);
    output_len = result.len;
    return true;
}

// New export function for tree JSON output
export fn parseExpressionToTree(input_ptr: [*]const u8, input_len: usize) bool {
    // Reset output
    output_len = 0;

    // Create a null-terminated string from the input
    if (input_len >= output_writer.len - 1) {
        // Input too long, copy error message
        const error_msg = "Error: Input too long";
        @memcpy(output_writer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    }

    // Copy input and null-terminate
    var input_writer: [2048]u8 = undefined;
    @memcpy(input_writer[0..input_len], input_ptr[0..input_len]);
    input_writer[input_len] = 0;
    const expr: [:0]const u8 = input_writer[0..input_len :0];

    // Create arena allocator
    var arena_impl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    // Parse the expression
    const result = parseExpressionToTreeInternal(expr, arena) catch |err| {
        const error_msg = switch (err) {
            error.UnexpectedToken => "Error: Unexpected token",
            error.ExpectedSomething => "Error: Expected something",
            error.UnmatchedParentheses => "Error: Unmatched parentheses",
            error.EmptyParentheses => "Error: Empty parentheses",
            error.InvalidOperator => "Error: Invalid operator",
            error.OutOfMemory => "Error: Out of memory",
            error.Overflow => "Error: Overflow",
            error.InvalidCharacter => "Error: Invalid character",
            error.WriteFailed => "Error: Write failed",
        };
        @memcpy(output_writer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    };

    // Copy result to output writer
    if (result.len >= output_writer.len) {
        const error_msg = "Error: Result too long";
        @memcpy(output_writer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    }

    @memcpy(output_writer[0..result.len], result);
    output_len = result.len;
    return true;
}

fn parseExpressionInternal(expr: [:0]const u8, allocator: std.mem.Allocator) ![]const u8 {
    var token_stream: tex.TokenStream = .empty;
    defer token_stream.deinit(allocator);

    var tokenizer = tex.Tokenizer.init(expr);

    while (tokenizer.next()) |token| {
        if (token.tag == .Eof) {
            break;
        }
        try token_stream.append(allocator, token);
    }

    var parser = tex.Parser.init(token_stream, expr, allocator);
    const ast = try parser.parse(0);

    var writer_impl: std.Io.Writer.Allocating = .init(allocator);
    defer writer_impl.deinit();

    try wasmPolishToString(&writer_impl.writer, &ast, expr);

    const result = std.mem.trim(u8, writer_impl.written(), " ");

    const result_copy = try allocator.dupe(u8, result);
    return result_copy;
}

fn parseExpressionToTreeInternal(expr: [:0]const u8, allocator: std.mem.Allocator) ![]const u8 {
    var token_stream: tex.TokenStream = .empty;
    defer token_stream.deinit(allocator);

    var tokenizer = tex.Tokenizer.init(expr);

    while (tokenizer.next()) |token| {
        if (token.tag == .Eof) {
            break;
        }
        try token_stream.append(allocator, token);
    }

    var parser = tex.Parser.init(token_stream, expr, allocator);
    const ast = try parser.parse(0);

    var writer_impl: std.Io.Writer.Allocating = .init(allocator);
    defer writer_impl.deinit();

    try wasmTreeToJson(&ast, expr, &writer_impl.writer);

    const result = std.mem.trim(u8, writer_impl.written(), " ");

    const result_copy = try allocator.dupe(u8, result);
    return result_copy;
}
