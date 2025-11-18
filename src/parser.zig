const std = @import("std");
const expect = std.testing.expect;
const ArrayList = std.ArrayList;

const tok = @import("tokenizer.zig");

const input = enum { Op, Atom, Invalid };

const Tag = enum { Number, Variable, BinaryOperation };

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

pub fn get_infix_operator(text: []const u8, tag: tok.TokenType) ?ExprType {
    switch (tag) {
        .Variable => {
            return .iMul;
        },
        .LParen => {
            return .Juxt;
        },
        else => {},
    }

    return infix_operators.get(text);
}

pub fn get_prefix_operator(text: []const u8) ?ExprType {
    return prefix_operators.get(text);
}

pub const Expression = struct {
    type: ExprType,
    value: ?union(enum) { i: i64, f: f64, length: usize },
    pos: tok.Loc,
    children: ?[*]Expression, // Might be null for literals
};

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

pub const Parser = struct {
    token_stream: tok.TokenStream,
    input: [:0]const u8,
    head: usize = 0,
    current: tok._Token,
    allocator: std.mem.Allocator,

    pub fn init(token_stream: tok.TokenStream, expr: [:0]const u8, allocator: std.mem.Allocator) Parser {
        return .{ .token_stream = token_stream, .input = expr, .head = 0, .current = .{ .tag = .Eof, .pos = .{ .from = 0, .to = 0 } }, .allocator = allocator };
    }

    pub fn consume(self: *Parser) void {
        if (self.head >= self.token_stream.items.len) {
            self.current = .{ .tag = .Eof, .pos = .{ .from = 0, .to = 0 } };
            return;
        }
        self.current = self.token_stream.items[self.head];
        std.log.debug("Current token: {s} text: {s}", .{ @tagName(self.current.tag), self.input[self.current.pos.from..self.current.pos.to] });
        self.head += 1;
    }

    pub fn peek(self: *Parser) tok._Token {
        if (self.head >= self.token_stream.items.len) {
            return .{ .tag = .Eof, .pos = .{ .from = 0, .to = 0 } };
        }
        std.log.debug("Peeking token: {s} text: {s}", .{ @tagName(self.token_stream.items[self.head].tag), self.input[self.token_stream.items[self.head].pos.from..self.token_stream.items[self.head].pos.to] });
        return self.token_stream.items[self.head];
    }

    pub fn expect(self: *Parser, tag: tok.TokenType, err: ParserError) ParserError!void {
        // This function consumes the current token and checks if it matches the expected tag.
        // Throws ParserError if the tag does not match.
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

        const op_text: []const u8 = self.input[op.pos.from..op.pos.to];
        const op_type: ?ExprType = get_prefix_operator(op_text);
        if (op_type == null) return ParserError.InvalidOperator;

        const r_bp = try prefix_binding_power(op_type);
        const expr = try self.parse(r_bp);

        const children = try self.allocator.alloc(Expression, 1); // Allocate memory for the children array
        children[0] = expr;

        return Expression{ .type = op_type.?, .value = null, .pos = op.pos, .children = children.ptr };
    }

    pub fn parse_paren(self: *Parser) ParserError!Expression {

        // writer.print("Entering paren", .{});
        // Paren can be for grouping (has .Comma) or simply to wrap an Expression
        // Lookahead for commas
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
        } // If there are no commas, it's a parenthesized Expression

        const len = commas + 1;
        var children = try self.allocator.alloc(Expression, len);
        for (0..len) |i| {
            children[i] = try self.parse(0);
            if (i < len - 1) try self.expect(.Comma, ParserError.UnmatchedParentheses); // Comma is expected between Expressions
        }

        return Expression{ .type = .Object, .value = .{ .length = len }, .pos = self.current.pos, .children = children.ptr };
    }

    pub fn parse_func(self: *Parser) ParserError!Expression {

        // Final type can be FunctionCall for f(x,y) or Juxt for \\sin x
        var final_type: ExprType = undefined;

        switch (self.peek().tag) {
            .LParen => {
                final_type = .FunctionCall;
            }, // \\sin(x)
            .Eof => {
                return ParserError.ExpectedSomething;
            }, // \\sin
            else => {
                final_type = .Juxt;
            }, // \\sin abc
        }

        const name = Expression{ .type = .FunctionName, .value = null, .pos = self.current.pos, .children = null };
        var args = try self.parse(0); // Will return an .Object
        args.type = .Arguments;
        args.value = .{ .length = 1 };

        var children = try self.allocator.alloc(Expression, 2);
        children[0] = name;
        children[1] = args;

        const func = Expression{ .type = .FunctionCall, .value = null, .pos = self.current.pos, .children = children.ptr };

        std.log.debug(">>>>{}", .{args});

        return func;
    }

    pub fn parse(self: *Parser, min_bp: i8) ParserError!Expression {
        self.consume(); // Consume the first token (likely an atom, but can be an operator too)
        // self.current now has that token

        var lhs: Expression =
            switch (self.current.tag) {
                .Integer => Expression{ .type = .Number, .value = .{ .i = try std.fmt.parseInt(i64, self.input[self.current.pos.from..self.current.pos.to], 10) }, .pos = self.current.pos, .children = null },
                .Real => Expression{ .type = .Number, .value = .{ .f = try std.fmt.parseFloat(f64, self.input[self.current.pos.from..self.current.pos.to]) }, .pos = self.current.pos, .children = null },
                .Variable => Expression{ .type = .Variable, .value = null, .pos = self.current.pos, .children = null },
                .Op => try self.parse_prefix(),
                .FunctionName => try self.parse_func(),
                .LParen => paren: {
                    const expr: Expression = try self.parse_paren(); // Parse the Expression inside parentheses
                    try self.expect(.RParen, ParserError.UnmatchedParentheses); // Consume the ')' token
                    break :paren expr;
                },

                else => return ParserError.UnexpectedToken,
            };

        // writer.print("Parsed lhs: {s} text: {s}", .{ @tagName(lhs.type), self.input[lhs.pos.from..lhs.pos.to] });

        while (true) {
            const op = self.peek();
            var skip_op: bool = false;
            switch (op.tag) {
                .Eof => break,
                .Op, .With => {}, // Allow these
                .Variable => {
                    skip_op = true;
                }, // Implicit multiplication
                .LParen => {
                    skip_op = true;
                }, // Juxtapose Expression
                .Comma => break, // Comma returns the current Expression
                .RParen => break, // Stop parsing on closing parenthesis
                else => return ParserError.UnexpectedToken,
            }

            // Convert TokenType to inputType
            const op_text: []const u8 = self.input[op.pos.from..op.pos.to];
            const op_type: ?ExprType = get_infix_operator(op_text, op.tag);
            const l_bp, const r_bp = try infix_binding_power(op_type);
            if (l_bp < min_bp) break;

            if (!skip_op) self.consume(); // Consume the operator token

            const rhs: Expression = try self.parse(r_bp);

            // Allocate memory for the children array
            const children = try self.allocator.alloc(Expression, 2);
            children[0] = lhs;
            children[1] = rhs;

            lhs = Expression{ .type = op_type.?, .value = null, .pos = op.pos, .children = children.ptr };
        }
        return lhs;
    }
};

test "Parser Polish notation" {
    const alloc = std.heap.smp_allocator;
    try testParser(alloc, "a+b", "(+ a b)");
    try testParser(alloc, "a*b+c", "(+ (* a b) c)");
    try testParser(alloc, "a+b*c", "(+ a (* b c))");
    try testParser(alloc, "2^3", "(^ 2 3)");
    try testParser(alloc, "-a", "(-u a)");
}

test "Parser Advanced Polish notation" {
    const alloc = std.heap.smp_allocator;
    try testParser(alloc, "-x^2-y^2", "(- (-u (^ x 2)) (^ y 2))");
    try testParser(alloc, "-a-b-c-d", "(- (- (- (-u a) b) c) d)");
    try testParser(alloc, "abc", "(*i (*i a b) c)");
    try testParser(alloc, "\\sin abc", "(call func (args (*i a b)))");
}

test "Parser Complex Expressions" {
    const alloc = std.heap.smp_allocator;
    // Test individual parts first
    try testParser(alloc, "xyz", "(*i (*i x y) z)");
    try testParser(alloc, "xyz^2", "(*i (*i x y) (^ z 2))");
    try testParser(alloc, "-xyz^2", "(*i (*i (-u x) y) (^ z 2))"); // Unary minus binds to first variable

    // Test abc^2 part
    try testParser(alloc, "abc^2", "(*i (*i a b) (^ c 2))");

    // Full complex Expression: -xyz^{2}-abc^{2}
    try testParser(alloc, "-xyz^2-abc^2", "(- (*i (*i (-u x) y) (^ z 2)) (*i (*i a b) (^ c 2)))");
}

test "Parser Edge cases" {
    const alloc = std.heap.smp_allocator;
    // Operator precedence tests
    try testParser(alloc, "a+b*c^d", "(+ a (* b (^ c d)))");
    try testParser(alloc, "a^b+c*d", "(+ (^ a b) (* c d))");
    try testParser(alloc, "a*b^c+d", "(+ (* a (^ b c)) d)");

    // Multiple unary operators
    try testParser(alloc, "--a", "(-u (-u a))");
    try testParser(alloc, "-a+b", "(+ (-u a) b)");
    try testParser(alloc, "a+-b", "(+ a (-u b))");

    // Mixed implicit and explicit multiplication
    try testParser(alloc, "2x", "(*i 2 x)"); // Number followed by variable works
    try testParser(alloc, "2*x", "(* 2 x)"); // Explicit multiplication
}

pub fn renderAST(writer: *std.Io.Writer, expr: *const Expression, source: []const u8) std.Io.Writer.Error!void {
    _ = source;
    try renderASTHelper(writer, expr, "", true);
}

fn testParser(gpa: std.mem.Allocator, source: [:0]const u8, expected_polish: []const u8) !void {
    var token_stream: tok.TokenStream = .empty;
    defer token_stream.deinit(gpa);

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var tokenizer = tok.Tokenizer.init(source);

    while (tokenizer.next()) |token| {
        if (token.tag == .Eof) {
            break;
        }
        try token_stream.append(arena, token);
        tokenizer.dump(&token);
    }

    var parser = Parser.init(token_stream, source, arena);
    const ast = try parser.parse(0);

    // Convert Polish notation to string
    var polish_writer: std.Io.Writer.Allocating = .init(arena);
    defer polish_writer.deinit();

    try renderPolish(&polish_writer.writer, &ast, source);

    // Trim trailing whitespace
    const actual_polish = std.mem.trim(u8, polish_writer.written(), " ");

    std.log.debug("Expected: '{s}'", .{expected_polish});
    std.log.debug("Actual:   '{s}'", .{actual_polish});

    try std.testing.expectEqualStrings(expected_polish, actual_polish);

    // Print success
    std.log.debug("Success: {s}", .{source});
}

pub fn renderPolish(writer: *std.Io.Writer, expr: *const Expression, source: []const u8) std.Io.Writer.Error!void {
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
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Sub => {
            try writer.print(" (-", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Mul => {
            try writer.print(" (*", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .iMul => {
            try writer.print(" (*i", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Div => {
            try writer.print(" (/", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Pow => {
            try writer.print(" (^", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Dot => {
            try writer.print(" (.", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Assignment => {
            try writer.print(" (=", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .With => {
            try writer.print(" (with ", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .UnaryMinus => {
            try writer.print(" (-u", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
            }
            try writer.print(")", .{});
        },
        .UnaryPlus => {
            try writer.print(" (+", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
            }
            try writer.print(")", .{});
        },
        .Object => {
            try writer.print(" (obj", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try renderPolish(writer, &children[i], source);
                }
            }
            try writer.print(")", .{});
        },
        .Arguments => {
            try writer.print(" (args", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try renderPolish(writer, &children[i], source);
                }
            }
            try writer.print(")", .{});
        },
        .Paren => {
            try writer.print(" (paren", .{});
            if (expr.children) |children| {
                const length = expr.value.?.length;
                for (0..length) |i| {
                    try renderPolish(writer, &children[i], source);
                }
            }
            try writer.print(")", .{});
        },
        .Comma => {
            try writer.print(", ", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
        },
        .FunctionCall => {
            try writer.print(" (call", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Juxt => {
            try writer.print(" (juxt", .{});
            if (expr.children) |children| {
                try renderPolish(writer, &children[0], source);
                try renderPolish(writer, &children[1], source);
            }
            try writer.print(")", .{});
        },
        .Invalid => {
            try writer.print("INVALID ", .{});
        },
    }
}

fn renderASTHelper(writer: *std.Io.Writer, expr: *const Expression, prefix: []const u8, is_last: bool) std.Io.Writer.Error!void {
    // Print current node with appropriate connector
    const connector = if (is_last) "+-- " else "|-- ";

    switch (expr.type) {
        .Variable => {
            try writer.print("{s}{s}Var\n", .{ prefix, connector });
        },
        .FunctionName => {
            try writer.print("{s}{s}FuncName\n", .{ prefix, connector });
        },
        .Number => {
            if (expr.value) |val| {
                switch (val) {
                    .i => |i| try writer.print("{s}{s}Number: {d}\n", .{ prefix, connector, i }),
                    .f => |f| try writer.print("{s}{s}Number: {d}\n", .{ prefix, connector, f }),
                    else => unreachable,
                }
            } else {
                try writer.print("{s}{s}Number: <no value>\n", .{ prefix, connector });
            }
        },
        .Add, .Sub, .Mul, .Div, .Dot, .Pow, .Juxt, .Comma, .With, .Assignment => {
            try writer.print("{s}{s}{s}\n", .{ prefix, connector, @tagName(expr.type) });
            if (expr.children) |children| {
                // Create new prefix: extend current with either spaces or vertical bar
                var new_prefix: [256]u8 = undefined;
                const extension = if (is_last) "    " else "|   ";
                const new_len = @min(prefix.len + 4, 252); // Leave room for extension
                @memcpy(new_prefix[0..prefix.len], prefix);
                @memcpy(new_prefix[prefix.len..new_len], extension);

                try renderASTHelper(writer, &children[0], new_prefix[0..new_len], false);
                try renderASTHelper(writer, &children[1], new_prefix[0..new_len], true);
            }
        },
        .Object, .Arguments, .Paren => {
            try writer.print("{s}{s}{s}\n", .{ prefix, connector, @tagName(expr.type) });
            if (expr.children) |children| {
                var new_prefix: [256]u8 = undefined;
                const extension = if (is_last) "    " else "|   ";
                const new_len = @min(prefix.len + 4, 252);
                @memcpy(new_prefix[0..prefix.len], prefix);
                @memcpy(new_prefix[prefix.len..new_len], extension);

                const length = expr.value.?.length;
                for (0..length) |i| {
                    const is_last_child = (i == length - 1);
                    try renderASTHelper(writer, &children[i], new_prefix[0..new_len], is_last_child);
                }
            }
        },
        .iMul => {
            try writer.print("{s}{s}Mul\n", .{ prefix, connector });
            if (expr.children) |children| {
                var new_prefix: [256]u8 = undefined;
                const extension = if (is_last) "    " else "|   ";
                const new_len = @min(prefix.len + 4, 252);
                @memcpy(new_prefix[0..prefix.len], prefix);
                @memcpy(new_prefix[prefix.len..new_len], extension);

                try renderASTHelper(writer, &children[0], new_prefix[0..new_len], false);
                try renderASTHelper(writer, &children[1], new_prefix[0..new_len], true);
            }
        },
        .FunctionCall => {
            try writer.print("{s}{s}Call\n", .{ prefix, connector });
            if (expr.children) |children| {
                var new_prefix: [256]u8 = undefined;
                const extension = if (is_last) "    " else "|   ";
                const new_len = @min(prefix.len + 4, 252);
                @memcpy(new_prefix[0..prefix.len], prefix);
                @memcpy(new_prefix[prefix.len..new_len], extension);

                try renderASTHelper(writer, &children[0], new_prefix[0..new_len], false);
                try renderASTHelper(writer, &children[1], new_prefix[0..new_len], true);
            }
        },
        .UnaryMinus, .UnaryPlus => {
            try writer.print("{s}{s}{s}\n", .{ prefix, connector, @tagName(expr.type) });
            if (expr.children) |children| {
                var new_prefix: [256]u8 = undefined;
                const extension = if (is_last) "    " else "|   ";
                const new_len = @min(prefix.len + 4, 252);
                @memcpy(new_prefix[0..prefix.len], prefix);
                @memcpy(new_prefix[prefix.len..new_len], extension);

                try renderASTHelper(writer, &children[0], new_prefix[0..new_len], true);
            }
        },
        .Invalid => {
            try writer.print("{s}{s}Invalid Expression\n", .{ prefix, connector });
        },
    }
}
