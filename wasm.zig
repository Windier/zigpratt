const std = @import("std");

// Import only the necessary parts from test.zig, avoiding debug prints
const ArrayList = std.ArrayList;

// Define the types we need locally to avoid importing debug functions
const TokenType = enum {
    Variable,
    Constant,
    Number,
    Integer,
    Real,
    ImaginaryUnit,
    Plus,
    Minus,
    Caret,
    Asterisk,
    Identifier,
    BinaryOp,
    Op,
    latex_command,
    Slash,
    LParen,
    RParen,
    LBrak,
    RBrak,
    LBrace,
    RBrace,
    With,
    For,
    OperatorName,
    Color,
    Left,
    Right,
    VBar,
    LVBar,
    RVBar,
    Comma,
    Invalid,
    FunctionName,
    Fraction,
    Eof,
};

pub const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "left(", .LParen },
    .{ "right)", .RParen },
    .{ "left[", .LBrak },
    .{ "right]", .RBrak },
    .{ "left{", .LBrace },
    .{ "right}", .RBrace },
    .{ "left|", .LVBar },
    .{ "right|", .RVBar },
    .{ "left", .Left },
    .{ "right", .Right },
    .{ "{", .LBrace },
    .{ "}", .RBrace },
    .{ "frac", .Fraction },
    .{ "operatorname", .OperatorName },
    .{ "with", .With },
    .{ "for", .For },
    .{ "rgb", .Color },
    .{ "sin", .FunctionName },
    .{ "cos", .FunctionName },
    .{ "tan", .FunctionName },
    .{ "theta", .Variable },
    .{ "alpha", .Variable },
    .{ "gamma", .Variable },
    .{ "pi", .Constant },
});

pub fn getKeyword(text: []const u8) ?TokenType {
    return keywords.get(text);
}

const Loc = struct { from: usize, to: usize };
const _Token = struct { tag: TokenType, pos: Loc };
const TokenStream = ArrayList(_Token);

const ExprType = enum {
    Add,
    Sub,
    Mul,
    iMul,
    Pow,
    Div,
    Dot,
    Juxt,
    Comma,
    With,
    Assignment,
    Paren,
    Arguments,
    FunctionCall,
    FunctionName,
    Object,
    UnaryMinus,
    UnaryPlus,
    Number,
    Variable,
    Invalid,
};

const Expression = struct {
    type: ExprType,
    value: ?union(enum) { i: i64, f: f64, length: u32 }, // Changed to u32 for wasm32
    pos: Loc,
    children: ?[*]Expression,
};

pub const infix_operators = std.StaticStringMap(ExprType).initComptime(.{
    .{ "+", .Add },
    .{ "-", .Sub },
    .{ "*", .Mul },
    .{ "/", .Div },
    .{ "^", .Pow },
    .{ ".", .Dot },
    .{ "=", .Assignment },
    .{ "with", .With },
});

pub const prefix_operators = std.StaticStringMap(ExprType).initComptime(.{
    .{ "+", .UnaryPlus },
    .{ "-", .UnaryMinus },
});

pub fn get_infix_operator(text: []const u8, tag: TokenType) ?ExprType {
    switch (tag) {
        .Variable => return .iMul,
        .LParen => return .Juxt,
        else => {},
    }
    return infix_operators.get(text);
}

pub fn get_prefix_operator(text: []const u8) ?ExprType {
    return prefix_operators.get(text);
}

fn infix_binding_power(op: ?ExprType) error{InvalidOperator}!struct { i8, i8 } {
    if (op == null) return error.InvalidOperator;
    switch (op.?) {
        .Comma => return .{ -1, -1 },
        .Add, .Sub => return .{ 3, 4 },
        .Mul, .Div => return .{ 5, 6 },
        .iMul => return .{ 5, 6 },
        .Juxt => return .{ 5, 6 },
        .Dot => return .{ 8, 7 },
        .Pow => return .{ 9, 8 },
        .With => return .{ 10, 9 },
        .Assignment => return .{ 2, 1 },
        else => return error.InvalidOperator,
    }
}

fn prefix_binding_power(op: ?ExprType) error{InvalidOperator}!i8 {
    if (op == null) return error.InvalidOperator;
    switch (op.?) {
        .UnaryMinus, .UnaryPlus => return 6,
        else => return error.InvalidOperator,
    }
}

// Simplified Tokenizer for WASM (without debug prints)
const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    const State = enum {
        start,
        identifier,
        variable,
        variable_subscript,
        latex_command,
        operator_name,
        builtin,
        plus,
        minus,
        int,
        period,
        number,
        decimal_number,
        invalid,
        unknown,
    };

    pub fn init(buffer: [:0]const u8) Tokenizer {
        return .{
            .buffer = buffer,
            .index = 0,
        };
    }

    pub fn next(self: *Tokenizer) ?_Token {
        var result: _Token = .{ .tag = undefined, .pos = .{
            .from = self.index,
            .to = undefined,
        } };

        if (self.index >= self.buffer.len) {
            return .{
                .tag = .Eof,
                .pos = .{
                    .from = self.index,
                    .to = self.index,
                },
            };
        }

        state: switch (State.start) {
            .start => switch (self.buffer[self.index]) {
                '0'...'9' => {
                    self.index += 1;
                    continue :state .number;
                },
                '.' => {
                    self.index += 1;
                    continue :state .period;
                },
                '\\' => {
                    self.index += 1;
                    result.pos.from = self.index;
                    continue :state .latex_command;
                },
                'a'...'z', 'A'...'Z' => {
                    result.tag = .Variable;
                    continue :state .variable;
                },
                '+', '-', '*', '/', '^', '=' => {
                    result.tag = .Op;
                    self.index += 1;
                },
                ',' => {
                    result.tag = .Comma;
                    self.index += 1;
                },
                '(' => {
                    result.tag = .LParen;
                    self.index += 1;
                },
                ')' => {
                    result.tag = .RParen;
                    self.index += 1;
                },
                '[' => {
                    result.tag = .LBrak;
                    self.index += 1;
                },
                ']' => {
                    result.tag = .RBrak;
                    self.index += 1;
                },
                '{' => {
                    result.tag = .LBrace;
                    self.index += 1;
                },
                '}' => {
                    result.tag = .RBrace;
                    self.index += 1;
                },
                '|' => {
                    result.tag = .VBar;
                    self.index += 1;
                },
                ' ' => {
                    self.index += 1;
                    result.pos.from = self.index;
                    continue :state .start;
                },
                else => {
                    result.tag = .Invalid;
                    self.index += 1;
                },
            },
            .number => {
                switch (self.buffer[self.index]) {
                    '0'...'9' => {
                        self.index += 1;
                        continue :state .number;
                    },
                    '.' => continue :state .decimal_number,
                    else => {
                        result.tag = .Integer;
                    },
                }
            },
            .decimal_number => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '0'...'9' => {
                        continue :state .decimal_number;
                    },
                    else => {
                        result.tag = .Real;
                    },
                }
            },
            .period => {
                switch (self.buffer[self.index]) {
                    '0'...'9' => continue :state .decimal_number,
                    else => {
                        result.tag = .Op;
                    },
                }
            },
            .variable => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '_' => continue :state .variable_subscript,
                    else => {
                        result.tag = .Variable;
                    },
                }
            },
            .variable_subscript => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '0'...'9', '{' => continue :state .variable_subscript,
                    '}' => {
                        self.index += 1;
                        result.tag = .Variable;
                    },
                    else => {
                        result.tag = .Variable;
                    },
                }
            },
            .operator_name => {
                switch (self.buffer[self.index]) {
                    '{' => {
                        self.index += 1;
                        result.pos.from = self.index;
                        continue :state .operator_name;
                    },
                    'a'...'z', 'A'...'Z' => {
                        self.index += 1;
                        continue :state .operator_name;
                    },
                    else => {
                        const text = self.buffer[result.pos.from..self.index];
                        if (getKeyword(text)) |tag| {
                            result.tag = tag;
                            result.pos.to = self.index;
                            self.index += 1;
                            return result;
                        }
                        result.tag = .Invalid;
                        result.pos.to = self.index;
                        return result;
                    },
                }
            },
            .latex_command => {
                self.index += 1;
                result.tag = .latex_command;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z' => continue :state .latex_command,
                    else => {
                        const text = self.buffer[result.pos.from..self.index];
                        if (getKeyword(text)) |tag| {
                            if (tag == .OperatorName) {
                                result.pos.from = self.index;
                                continue :state .operator_name;
                            }
                            result.tag = tag;
                            result.pos.to = self.index;
                            return result;
                        }
                        result.tag = .Invalid;
                        result.pos.to = self.index;
                        return result;
                    },
                }
            },
            .invalid => {
                result.tag = .Invalid;
            },
            else => {
                result.tag = .Invalid;
            },
        }

        result.pos.to = self.index;
        return result;
    }
};

const Parser = struct {
    token_stream: TokenStream,
    expr: [:0]const u8,
    head: usize = 0,
    current: _Token,
    allocator: std.mem.Allocator,

    pub fn init(token_stream: TokenStream, expr: [:0]const u8, allocator: std.mem.Allocator) Parser {
        return .{ .token_stream = token_stream, .expr = expr, .head = 0, .current = .{ .tag = .Eof, .pos = .{ .from = 0, .to = 0 } }, .allocator = allocator };
    }

    pub fn consume(self: *Parser) void {
        if (self.head >= self.token_stream.items.len) {
            self.current = .{ .tag = .Eof, .pos = .{ .from = 0, .to = 0 } };
            return;
        }
        self.current = self.token_stream.items[self.head];
        self.head += 1;
    }

    pub fn peek(self: *Parser) _Token {
        if (self.head >= self.token_stream.items.len) {
            return .{ .tag = .Eof, .pos = .{ .from = 0, .to = 0 } };
        }
        return self.token_stream.items[self.head];
    }

    pub fn expect(self: *Parser, tag: TokenType, err: ParserError) ParserError!void {
        self.consume();
        if (self.current.tag != tag) {
            return err;
        }
    }

    pub const ParserError = error{
        UnexpectedToken,
        ExpectedSomething,
        UnmatchedParentheses,
        EmptyParentheses,
        InvalidOperator,
        OutOfMemory,
        Overflow,
        InvalidCharacter,
    };

    pub fn parse_prefix(self: *Parser) ParserError!Expression {
        const op = self.current;
        if (op.tag != .Op) return ParserError.UnexpectedToken;

        const op_text: []const u8 = self.expr[op.pos.from..op.pos.to];
        const op_type: ?ExprType = get_prefix_operator(op_text);
        if (op_type == null) return ParserError.InvalidOperator;

        const r_bp = try prefix_binding_power(op_type);
        const expr = try self.parse(r_bp);

        const children = try self.allocator.alloc(Expression, 1);
        children[0] = expr;

        return Expression{ .type = op_type.?, .value = null, .pos = op.pos, .children = children.ptr };
    }

    pub fn parse_paren(self: *Parser) ParserError!Expression {
        var level: i32 = 0;
        var commas: u32 = 0;
        for (self.token_stream.items[self.head..]) |token| {
            switch (token.tag) {
                .LParen => {
                    level -= 1;
                },
                .RParen => {
                    if (level == 0) break;
                    level += 1;
                },
                .Comma => {
                    if (level == 0) commas += 1;
                },
                else => {},
            }
        }

        if (level != 0) return ParserError.UnmatchedParentheses;
        if (commas == 0) {
            var children = try self.allocator.alloc(Expression, 1);
            children[0] = try self.parse(0);
            return Expression{ .type = .Paren, .value = .{ .length = 1 }, .pos = self.current.pos, .children = children.ptr };
        }

        const len = commas + 1;
        var children = try self.allocator.alloc(Expression, len);
        for (0..len) |i| {
            children[i] = try self.parse(0);
            if (i < len - 1) try self.expect(.Comma, ParserError.UnmatchedParentheses);
        }

        return Expression{ .type = .Object, .value = .{ .length = len }, .pos = self.current.pos, .children = children.ptr };
    }

    pub fn parse_func(self: *Parser) ParserError!Expression {
        var final_type: ExprType = undefined;

        switch (self.peek().tag) {
            .LParen => {
                final_type = .FunctionCall;
            },
            .Eof => {
                return ParserError.ExpectedSomething;
            },
            else => {
                final_type = .Juxt;
            },
        }

        const name = Expression{ .type = .FunctionName, .value = null, .pos = self.current.pos, .children = null };
        var args = try self.parse(0);
        args.type = .Arguments;
        args.value = .{ .length = 1 };

        var children = try self.allocator.alloc(Expression, 2);
        children[0] = name;
        children[1] = args;

        const func = Expression{ .type = .FunctionCall, .value = null, .pos = self.current.pos, .children = children.ptr };

        return func;
    }

    pub fn parse(self: *Parser, min_bp: i8) ParserError!Expression {
        self.consume();

        var lhs: Expression =
            switch (self.current.tag) {
                .Integer => Expression{ .type = .Number, .value = .{ .i = try std.fmt.parseInt(i64, self.expr[self.current.pos.from..self.current.pos.to], 10) }, .pos = self.current.pos, .children = null },
                .Real => Expression{ .type = .Number, .value = .{ .f = try std.fmt.parseFloat(f64, self.expr[self.current.pos.from..self.current.pos.to]) }, .pos = self.current.pos, .children = null },
                .Variable => Expression{ .type = .Variable, .value = null, .pos = self.current.pos, .children = null },
                .Op => try self.parse_prefix(),
                .FunctionName => try self.parse_func(),
                .LParen => paren: {
                    const expr: Expression = try self.parse_paren();
                    try self.expect(.RParen, ParserError.UnmatchedParentheses);
                    break :paren expr;
                },
                else => return ParserError.UnexpectedToken,
            };

        while (true) {
            const op = self.peek();
            var skip_op: bool = false;
            switch (op.tag) {
                .Eof => break,
                .Op, .With => {},
                .Variable => {
                    skip_op = true;
                },
                .LParen => {
                    skip_op = true;
                },
                .Comma => break,
                .RParen => break,
                else => return ParserError.UnexpectedToken,
            }

            const op_text: []const u8 = self.expr[op.pos.from..op.pos.to];
            const op_type: ?ExprType = get_infix_operator(op_text, op.tag);
            const l_bp, const r_bp = try infix_binding_power(op_type);
            if (l_bp < min_bp) break;

            if (!skip_op) self.consume();

            const rhs: Expression = try self.parse(r_bp);

            const children = try self.allocator.alloc(Expression, 2);
            children[0] = lhs;
            children[1] = rhs;

            lhs = Expression{ .type = op_type.?, .value = null, .pos = op.pos, .children = children.ptr };
        }
        return lhs;
    }
};

// WASM memory for string exchange
var output_buffer: [2048]u8 = undefined;
var output_len: usize = 0;

// Export function to get the output buffer pointer
export fn getOutputPtr() [*]u8 {
    return &output_buffer;
}

// Export function to get the output length
export fn getOutputLen() usize {
    return output_len;
}

// Simplified polishToString for WASM
fn wasmPolishToString(expr: *const Expression, source: []const u8, buffer: *std.ArrayList(u8)) !void {
    const writer = buffer.writer();
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
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Sub => {
            try writer.print(" (-", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Mul => {
            try writer.print(" (*", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .iMul => {
            try writer.print(" (*i", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Div => {
            try writer.print(" (/", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Pow => {
            try writer.print(" (^", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Dot => {
            try writer.print(" (.", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Assignment => {
            try writer.print(" (=", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .With => {
            try writer.print(" (with ", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .UnaryMinus => {
            try writer.print(" (-u", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
            }
            try writer.print(")", .{});
        },
        .UnaryPlus => {
            try writer.print(" (+", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Object => {
            try writer.print(" (obj", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try wasmPolishToString(&children[i], source, buffer);
                }
            }
            try writer.print(")", .{});
        },
        .Arguments => {
            try writer.print(" (args", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try wasmPolishToString(&children[i], source, buffer);
                }
            }
            try writer.print(")", .{});
        },
        .Paren => {
            try writer.print(" (paren", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try wasmPolishToString(&children[i], source, buffer);
                }
            }
            try writer.print(")", .{});
        },
        .Comma => {
            try writer.print(", ", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
        },
        .FunctionCall => {
            try writer.print(" (call", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Juxt => {
            try writer.print(" (juxt", .{});
            if (expr.children) |children| {
                try wasmPolishToString(&children[0], source, buffer);
                try wasmPolishToString(&children[1], source, buffer);
            }
            try writer.print(")", .{});
        },
        .Invalid => {
            try writer.print("INVALID ", .{});
        },
    }
}

export fn parseExpression(input_ptr: [*]const u8, input_len: usize) bool {
    // Reset output
    output_len = 0;
    
    // Create a null-terminated string from the input
    if (input_len >= output_buffer.len - 1) {
        // Input too long, copy error message
        const error_msg = "Error: Input too long";
        @memcpy(output_buffer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    }
    
    // Copy input and null-terminate
    var input_buffer: [2048]u8 = undefined;
    @memcpy(input_buffer[0..input_len], input_ptr[0..input_len]);
    input_buffer[input_len] = 0;
    const expr: [:0]const u8 = input_buffer[0..input_len :0];
    
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
        };
        @memcpy(output_buffer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    };
    
    // Copy result to output buffer
    if (result.len >= output_buffer.len) {
        const error_msg = "Error: Result too long";
        @memcpy(output_buffer[0..error_msg.len], error_msg);
        output_len = error_msg.len;
        return false;
    }
    
    @memcpy(output_buffer[0..result.len], result);
    output_len = result.len;
    return true;
}

fn parseExpressionInternal(expr: [:0]const u8, allocator: std.mem.Allocator) ![]const u8 {
    var token_stream: TokenStream = TokenStream.init(allocator);
    defer token_stream.deinit();
    
    var tokenizer = Tokenizer.init(expr);
    
    while (tokenizer.next()) |token| {
        if (token.tag == .Eof) {
            break;
        }
        try token_stream.append(token);
    }
    
    var parser = Parser.init(token_stream, expr, allocator);
    const ast = try parser.parse(0);
    
    var polish_buffer = ArrayList(u8).init(allocator);
    try wasmPolishToString(&ast, expr, &polish_buffer);
    
    const result = std.mem.trim(u8, polish_buffer.items, " ");
    
    const result_copy = try allocator.dupe(u8, result);
    return result_copy;
}
