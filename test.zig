const std = @import("std");

const print = std.debug.print;
const expect = std.testing.expect;
const Allocator = std.heap.page_allocator;
const ArrayList = std.ArrayList;

// a + b -- Add(a,b)
// f = (x) -> x + 1 -- Assignment of lambda function
// f(x) = x + 1 -- FunctionCall
// abc.sin -- lhs / dot / rhs
// a.x -- DotAccess
// (x) + 1
// (x) => x - 1
// [a, b, c].join()
// [a, b, c] = f()

// fn dfdx(k: usize, x: *f32) void {
//     x.* += @floatFromInt(k);
// }

// Lexer Tokens
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
    latex_command,
    Slash,
    LParen,
    RParen,
    LBrak,
    RBrak,
    LBrace,
    RBrace,
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

const Loc = struct { from: usize, to: usize};
const _Token = struct { tag: TokenType, pos: Loc };

const TokenStream = ArrayList(_Token);

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    const State = enum {
        start,
        identifier,
        variable,
        variable_subscript,
        latex_command,
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

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // print("Initial string, {s}, len: {d}\n", .{buffer, buffer.len});
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
            // print("Reached end of expression\n", .{});
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
                // '+', '-', '*', '/', '^' => {
                //     result.tag = .BinaryOp;
                //     self.index += 1;
                // },
                '+' => {
                    result.tag = .Plus;
                    self.index += 1;
                },
                '-' => {
                    result.tag = .Minus;
                    self.index += 1;
                },
                '*' => {
                    result.tag = .Asterisk;
                    self.index += 1;
                },
                '/' => {
                    result.tag = .Slash;
                    self.index += 1;
                },
                '^' => {
                    result.tag = .Caret;
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
                    else => {},
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
            .latex_command => {
                self.index += 1;
                result.tag = .latex_command;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z' => continue :state .latex_command,
                    else => {
                        const text = self.buffer[result.pos.from..self.index];
                        if (getKeyword(text)) |tag| {
                            // print("Found keyword: {s}\n", .{text});
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

const Expr = enum {
  Op,
  Atom,
  Invalid
};

const Tag = enum {
  Number,
  Variable,
  BinaryOperation
};

const ExprType = enum {
    Add,
    Sub,
    Mul,
    Div,
    Number,
    Invalid,
};

const Expression = struct {
  type: ExprType,
  value: ? union {i: i64, f: f64},
  pos: Loc,
  children: ?[*]Expression // Might be null for literals
};

fn infixBindingPower(op: TokenType) !struct { u8, u8 } {
    switch (op) {
        .Plus, .Minus => return .{ 1, 2 },
        .Asterisk, .Slash => return .{ 3, 4 },
        else => @panic("bad op"),
    }
}

pub fn Parser(token_stream: TokenStream, _: [:0]const u8, head: usize, allocator: std.mem.Allocator) !Expression {

  var index = head;
  var token: _Token = undefined;

  token = token_stream.items[index];

  const lhs: Expression = switch (token.tag){
    .Integer => Expression{ .type = .Number, .value = .{ .i = 1 }, .pos = token.pos, .children = null},
    else => Expression{ .type = .Invalid, .value = null, .pos = token.pos, .children = null},
  };

  index += 1; // This should be peek, not advance
  token = token_stream.items[index];
  const l_bp, const r_bp = try infixBindingPower(token.tag);
  std.debug.print("LBP: {}, RBP: {}\n", .{ l_bp, r_bp }); 

  const op: ExprType = switch (token.tag){
    .BinaryOp => .Add,
    else => .Invalid,
  };

  index += 1;
  token = token_stream.items[index];
  const rhs = switch (token.tag){
    .Integer => Expression{ .type = .Number, .value = .{ .i = 1 }, .pos = token.pos, .children = null},
    else => Expression{ .type = .Invalid, .value = null, .pos = token.pos, .children = null},
  };

  // Allocate memory for the children array
  const children = try allocator.alloc(Expression, 2);
  children[0] = lhs;
  children[1] = rhs;

  return Expression{ 
    .type = op, 
    .value = null, 
    .pos = .{ .from = 0, .to = 0 }, 
    .children = children.ptr 
  };

}

const ParserS = struct {
    token_stream: TokenStream,
    expr: [:0]const u8,
    head: usize,
    current: _Token,
    allocator: std.mem.Allocator,

    pub fn init( token_stream: TokenStream, expr: [:0]const u8, allocator: std.mem.Allocator ) ParserS {
      return .{ .token_stream = token_stream, .expr = expr, .head = 0, .allocator = allocator};
    }

    pub fn advance(self: *ParserS) Token_ {
      self.head += 1;
      if (self.head  >= token_stream.len) {
        return Token_.Eof;
      }
      return token_stream[self.head];
    }

    pub fn peek(self: *ParserS) Token_ {
      const next_index = self.head + 1;
      if (next_index  >= token_stream.len) {
        return Token_.Eof;
      }
      return token_stream[next_index];
    }

    pub fn parse(self: *ParserS, token_stream: TokenStream, _: [:0]const u8, allocator: std.mem.Allocator) !Expression {

      var index = self.head;
      self.current = token_stream.items[index];

      const lhs: Expression = 
        switch (self.current.tag){
          .Integer => Expression{ .type = .Number, .value = .{ .i = 1 }, .pos = self.current.pos, .children = null},
          else => Expression{ .type = .Invalid, .value = null, .pos = self.current.pos, .children = null},
        };

      index += 1; // This should be peek, not advance
      const op = token_stream.items[index];
      const l_bp, const r_bp = try infixBindingPower(op.tag);
      std.debug.print("LBP: {}, RBP: {}\n", .{ l_bp, r_bp }); 

      index += 1;
      self.current = token_stream.items[index];
      const rhs = switch (self.current.tag){
        .Integer => Expression{ .type = .Number, .value = .{ .i = 1 }, .pos = self.current.pos, .children = null},
        else => Expression{ .type = .Invalid, .value = null, .pos = self.current.pos, .children = null},
      };

      // Allocate memory for the children array
      const children = try allocator.alloc(Expression, 2);
      children[0] = lhs;
      children[1] = rhs;

      return Expression{ 
        .type = op, 
        .value = null, 
        .pos = .{ .from = 0, .to = 0 }, 
        .children = children.ptr 
      };
    }

    
}

    // pub fn peek(self: *ParserS) ?_Token {
    // 	if (self.head + 1 >= self.token_stream.items.len) {
    // 		return TokenType.Eof;
    // 	}
    // 	return self.token_stream.items[self.head];
    // }

    // pub fn advance(self: *ParserS) ?

pub fn main() !void {
    // Initialize the parsing rules

    var token_stream: TokenStream = TokenStream.init(Allocator);
    defer token_stream.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();


    const expr: [:0]const u8 = "3 + 4 * 2 - 1";
    var tokenizer = Tokenizer.init(expr);
    print("-- start -- : {s}\n", .{expr});

    while (tokenizer.next()) |token| {
      if (token.tag == .Eof) { print("--eof--\n", .{}); break; }
      try token_stream.append(token);
      tokenizer.dump(&token);
    }

    const ast = try Parser(token_stream, expr, 0, allocator);
    print("AST: {}\n", .{ast});



 
    // print("Successfully parsed expression into AST\n");
    // printAST(ast, 0);
}



test "Testing Tokenizer" {
    try testTokenize("3", &.{.Integer});
    try testTokenize(".32342", &.{.Real});
    try testTokenize("a_{1}", &.{.Variable});
    try testTokenize("\\sin", &.{.FunctionName});
    try testTokenize("sin", &.{ .Variable, .Variable, .Variable });
    try testTokenize("a_{1}+\\sin*3", &.{ .Variable, .Plus, .FunctionName, .Asterisk, .Integer });
    try testTokenize("\\sin", &.{.FunctionName});
    try testTokenize("\\cos", &.{.FunctionName});
    try testTokenize("abc_{123}", &.{ .Variable, .Variable, .Variable });
    try testTokenize("a_{1}+\\sin*3.25-2.2.3.3", &.{ .Variable, .Plus, .FunctionName, .Asterisk, .Real, .Minus, .Real, .Real, .Real });
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

test "Complex expression with variables and functions" {
    try testTokenize("a_{1}+\\sin*3", &.{ .Variable, .Plus, .FunctionName, .Asterisk, .Integer });
}

test "Multiple character variables" {
    try testTokenize("abc_{123}", &.{ .Variable, .Variable, .Variable });
}

test "Complex mathematical expression" {
    try testTokenize("a_{1}+\\sin*3.25-2.2.3.3", &.{ .Variable, .Plus, .FunctionName, .Asterisk, .Real, .Minus, .Real, .Real, .Real });
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


fn testTokenize(source: [:0]const u8, expected_token_tags: []const TokenType) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected_token_tags) |expected_token_tag| {
        const token = tokenizer.next();
        tokenizer.dump(&token);
        try std.testing.expectEqual(expected_token_tag, token.tag);
    }

    const last_token = tokenizer.next();
    try std.testing.expectEqual(TokenType.Eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.pos.from);
    try std.testing.expectEqual(source.len, last_token.pos.to);

    // Print success
    print("Success: {s}\n", .{source});
}

