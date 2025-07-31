const std = @import("std");

const TokenType = enum {
    Number,
    Plus,
    Minus,
    Star,
    Slash,
    Eof,
};

const Token = struct {
    ttype: TokenType,
    value: ?i64 = null,
};

const AstNode = union(enum) {
    number: i64,
    binary_op: struct {
        left: *AstNode,
        op: TokenType,
        right: *AstNode,
    },

    pub fn print(self: *const AstNode, allocator: std.mem.Allocator, indent: usize) !void {
        const spaces = try allocator.alloc(u8, indent * 2);
        defer allocator.free(spaces);
        @memset(spaces, ' ');

        switch (self.*) {
            .number => |n| {
                std.debug.print("{s}Number: {}\n", .{ spaces, n });
            },
            .binary_op => |op| {
                const op_str = switch (op.op) {
                    .Plus => "Add",
                    .Minus => "Sub",
                    .Star => "Mul",
                    .Slash => "Div",
                    else => "Unknown",
                };
                std.debug.print("{s}{s}\n", .{ spaces, op_str });
                try op.left.print(allocator, indent + 1);
                try op.right.print(allocator, indent + 1);
            },
        }
    }

    pub fn deinit(self: *AstNode, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .number => {},
            .binary_op => |op| {
                op.left.deinit(allocator);
                op.right.deinit(allocator);
                allocator.destroy(op.left);
                allocator.destroy(op.right);
            },
        }
    }
};

const Lexer = struct {
    input: []const u8,
    pos: usize = 0,

    pub fn next(self: *Lexer) Token {
        while (self.pos < self.input.len and self.input[self.pos] == ' ') self.pos += 1;
        if (self.pos >= self.input.len) return .{ .ttype = .Eof };

        const c = self.input[self.pos];
        self.pos += 1;

        return switch (c) {
            '+' => .{ .ttype = .Plus },
            '-' => .{ .ttype = .Minus },
            '*' => .{ .ttype = .Star },
            '/' => .{ .ttype = .Slash },
            '0'...'9' => {
                var value: i64 = c - '0';
                while (self.pos < self.input.len and std.ascii.isDigit(self.input[self.pos])) {
                    value = value * 10 + (self.input[self.pos] - '0');
                    self.pos += 1;
                }
                return .{ .ttype = .Number, .value = value };
            },
            else => std.debug.panic("Unexpected character: {}", .{c}),
        };
    }
};

const Parser = struct {
    lexer: *Lexer,
    current: Token,
    allocator: std.mem.Allocator,

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) Parser {
        return .{
            .lexer = lexer,
            .current = lexer.next(),
            .allocator = allocator,
        };
    }

    fn advance(self: *Parser) void {
			        self.current = self.lexer.next();
							    }
		}

    fn get_precedence(ttype: TokenType) u8 {
        return switch (ttype) {
            .Plus, .Minus => 10,
            .Star, .Slash => 20,
            else => 0,
        };
    }

    pub fn parse_expression(self: *Parser, precedence: u8) !*AstNode {
        var left = switch (self.current.ttype) {
            .Number => blk: {
                const value = self.current.value.?;
                self.advance();
                const node = try self.allocator.create(AstNode);
                node.* = AstNode{ .number = value };
                break :blk node;
            },
            else => std.debug.panic("Expected number, got {}", .{self.current.ttype}),
        };

        while (get_precedence(self.current.ttype) > precedence) {
            const op = self.current.ttype;
            const next_precedence = get_precedence(op);
            self.advance();
            const right = try self.parse_expression(next_precedence);

            const binary_node = try self.allocator.create(AstNode);
            binary_node.* = AstNode{ .binary_op = .{
                .left = left,
                .op = op,
                .right = right,
            } };
            left = binary_node;
        }

        return left;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = "1 + 2 * 3 - 4 / 5";
    var lexer = Lexer{ .input = input };
    var parser = Parser.init(&lexer, allocator);

    // Start timing
    const start_time = std.time.nanoTimestamp();

    const ast = try parser.parse_expression(0);

    // End timing
    const end_time = std.time.nanoTimestamp();
    const parse_time_ns = end_time - start_time;
    const parse_time_ms = @as(f64, @floatFromInt(parse_time_ns)) / 1_000_000.0;

    defer {
        ast.deinit(allocator);
        allocator.destroy(ast);
    }

    // std.debug.print("Expression: {s}\n", .{input});
    std.debug.print("Parse time: {d:.6} milliseconds\n", .{parse_time_ms});
    // std.debug.print("AST:\n", .{});
    // try ast.print(allocator, 0);
}
