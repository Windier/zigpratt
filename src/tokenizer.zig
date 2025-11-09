const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const ArrayList = std.ArrayList;

// Lexer Tokens
pub const TokenType = enum {
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
// \frac{}{}
pub const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "left(", .LParen },
    .{ "right)", .RParen },
    .{ "left[", .LBrak },
    .{ "right]", .RBrak },
    .{ "left{", .LBrace },
    .{ "right}", .RBrace },
    .{ "left|", .LVBar },
    .{ "right|", .RVBar },
    .{ "left", .Left }, // latex: \left
    .{ "right", .Right }, // latex: \right
    .{ "{", .LBrace }, // latex: \{
    .{ "}", .RBrace }, // latex : \}
    .{ "frac", .Fraction },
    .{ "operatorname", .OperatorName }, // LaTeX: \operatorname{with}
    .{ "with", .With }, // LaTeX: \operatorname{with}
    .{ "for", .For }, // LaTeX: \operatorname{for}
    .{ "rgb", .Color }, // LaTeX: \operatorname{rgb}

    .{ "sin", .FunctionName },
    .{ "cos", .FunctionName },
    .{ "tan", .FunctionName },

    .{ "theta", .Variable },
    .{ "alpha", .Variable },
    .{ "gamma", .Variable },
    .{ "pi", .Constant },
});

const Token = struct { type: TokenType, text: ?u8 = null, value: ?i64 = null };

pub fn getKeyword(text: []const u8) ?TokenType {
    return keywords.get(text);
}

pub const Loc = struct { from: usize, to: usize };
pub const _Token = struct { tag: TokenType, pos: Loc };

pub const TokenStream = ArrayList(_Token);

pub const Tokenizer = struct {
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
        number, // integer
        decimal_number, // float
        invalid,
        unknown,
    };

    pub fn dump(self: *Tokenizer, token: *const _Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.pos.from..(token.pos.to)] });
    }

    pub fn init(buf: [:0]const u8) Tokenizer {
        // print("Initial string, {s}, len: {d}\n", .{writer, writer.len});
        return .{
            .buffer = buf,
            .index = 0,
        };
    }

    pub fn next(self: *Tokenizer) ?_Token {
        var result: _Token = .{ .tag = undefined, .pos = .{
            .from = self.index,
            .to = undefined,
        } };

        if (self.index >= self.buffer.len) {
            // print("Reached end of Expression\n", .{});
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
                    result.pos.from = self.index; // ignore the backslash
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
                    self.index += 1; // skip whitespace
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
                    '0'...'9' => { // this will consume numbers in "123.23" before the decimal
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
                    '0'...'9' => { // this will consume numbers after the decimal point
                        // self.index += 1;
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
            .variable => { // If we're here, then we're past the initial letter in a_{123}
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
                        result.pos.from = self.index; // ignore the opening brace
                        continue :state .operator_name;
                    },
                    'a'...'z', 'A'...'Z' => {
                        self.index += 1;
                        continue :state .operator_name;
                    },
                    else => {
                        const text = self.buffer[result.pos.from..self.index];
                        print("Keyword: {s}\n", .{text});
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
                            print("Keyword found: {s} -> {s}\n", .{ text, @tagName(tag) });
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

fn testTokenize(source: [:0]const u8, expected_token_tags: []const TokenType) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected_token_tags) |expected_token_tag| {
        const token = tokenizer.next().?; // Unwrap the optional
        tokenizer.dump(&token);
        try std.testing.expectEqual(expected_token_tag, token.tag);
    }

    const last_token = tokenizer.next().?; // Unwrap the optional
    try std.testing.expectEqual(TokenType.Eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.pos.from);
    try std.testing.expectEqual(source.len, last_token.pos.to);

    // Print success
    print("Success: {s}\n", .{source});
}

test "Testing Tokenizer" {
    try testTokenize("3", &.{.Integer});
    try testTokenize(".32342", &.{.Real});
    try testTokenize("a_{1}", &.{.Variable});
    try testTokenize("\\sin", &.{.FunctionName});
    try testTokenize("sin", &.{ .Variable, .Variable, .Variable });
    try testTokenize("a_{1}+\\sin*3", &.{ .Variable, .Op, .FunctionName, .Op, .Integer });
    try testTokenize("\\sin", &.{.FunctionName});
    try testTokenize("\\cos", &.{.FunctionName});
    try testTokenize("abc_{123}", &.{ .Variable, .Variable, .Variable });
    try testTokenize("a_{1}+\\sin*3.25-2.2.3.3", &.{ .Variable, .Op, .FunctionName, .Op, .Real, .Op, .Real, .Real, .Real });
    try testTokenize("a_{abc}", &.{.Variable});
    try testTokenize("\\frac{a_{1}}{2}", &.{ .Fraction, .LBrace, .Variable, .RBrace, .LBrace, .Integer, .RBrace });
    try testTokenize("\\left\\{1,2,3\\right\\}", &.{ .Left, .LBrace, .Integer, .Comma, .Integer, .Comma, .Integer, .Right, .RBrace });
}

test "Integer tokenization" {
    try testTokenize("3", &.{.Integer});
}

test "Real number tokenization" {
    try testTokenize(".32342", &.{.Real});
}

test "Variable with subscript" {
    try testTokenize("a_{1}", &.{.Variable});
}

test "LaTeX function name" {
    try testTokenize("\\sin", &.{.FunctionName});
    try testTokenize("\\cos", &.{.FunctionName});
}

test "Regular variable tokenization" {
    try testTokenize("sin", &.{ .Variable, .Variable, .Variable });
}

test "Complex Expression with variables and functions" {
    try testTokenize("a_{1}+\\sin*3", &.{ .Variable, .Op, .FunctionName, .Op, .Integer });
}

test "Multiple character variables" {
    try testTokenize("abc_{123}", &.{ .Variable, .Variable, .Variable });
}

test "Complex mathematical Expression" {
    try testTokenize("a_{1}+\\sin*3.25-2.2.3.3", &.{ .Variable, .Op, .FunctionName, .Op, .Real, .Op, .Real, .Real, .Real });
}

test "Variable with text subscript" {
    try testTokenize("a_{abc}", &.{.Variable});
}

test "LaTeX fraction command" {
    try testTokenize("\\frac{a_{1}}{2}", &.{ .Fraction, .LBrace, .Variable, .RBrace, .LBrace, .Integer, .RBrace });
}

test "LaTeX left-right delimiters" {
    try testTokenize("\\left\\{1,2,3\\right\\}", &.{ .Left, .LBrace, .Integer, .Comma, .Integer, .Comma, .Integer, .Right, .RBrace });
}
