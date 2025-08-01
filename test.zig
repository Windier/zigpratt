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
    Op,
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

const Loc = struct { from: usize, to: usize };
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
                '+', '-', '*', '/', '^' => {
                    result.tag = .Op;
                    self.index += 1;
                },
                // '+' => {
                //     result.tag = .Plus;
                //     self.index += 1;
                // },
                // '-' => {
                //     result.tag = .Minus;
                //     self.index += 1;
                // },
                // '*' => {
                //     result.tag = .Asterisk;
                //     self.index += 1;
                // },
                // '/' => {
                //     result.tag = .Slash;
                //     self.index += 1;
                // },
                // '^' => {
                //     result.tag = .Caret;
                //     self.index += 1;
                // },
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

const Expr = enum { Op, Atom, Invalid };

const Tag = enum { Number, Variable, BinaryOperation };

const ExprType = enum {
    Add,
    Sub,
    Mul,
    iMul,
    Pow,
    Div,
    Dot,
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
  .{ "^", .Pow }, // iMul is used for implicit multiplication, e.g., ab is a iMul b
  .{ ".", .Dot},
});

pub const prefix_operators = std.StaticStringMap(ExprType).initComptime(.{
  .{ "+", .UnaryPlus },
  .{ "-", .UnaryMinus },
});

pub fn getInfixOperator(text: []const u8, tag: TokenType) ?ExprType {
    if (tag == .Variable) return .iMul;
    return infix_operators.get(text);
}

pub fn getPrefixOperator(text: []const u8) ?ExprType {
    return prefix_operators.get(text);
}


const Expression = struct {
    type: ExprType,
    value: ?union(enum) { i: i64, f: f64 },
    pos: Loc,
    children: ?[*]Expression, // Might be null for literals
};

// const Errors = error {
//   UnrecognizedOp 
// }

fn infixBindingPower(op: ?ExprType) error{InvalidOperator}!struct { u8, u8 } {
  if (op == null) return error.InvalidOperator;
  switch (op.?) {
    .Add, .Sub => return .{ 1, 2 },
    .Mul, .Div => return .{ 3, 4 },
    .iMul => return .{3, 4},
    .Dot => return .{ 6, 5 },
    .Pow => return .{ 7, 6 },
    else => return error.InvalidOperator,
  }
}

fn prefixBindingPower(op: ?ExprType) error{InvalidOperator}!u8 {
  if (op == null) return error.InvalidOperator;
  switch (op.?) {
    .UnaryMinus, .UnaryPlus => return 6,
    else => return error.InvalidOperator,
  }
}

const Parser = struct {
  token_stream: TokenStream,
  expr: [:0]const u8,
  head: usize = 0,
  current: _Token,
  allocator: std.mem.Allocator,

  pub fn init(token_stream: TokenStream, expr: [:0]const u8, allocator: std.mem.Allocator) Parser {
    return .{ 
      .token_stream = token_stream, 
      .expr = expr, 
      .head = 0, 
      .current = .{ .tag = .Eof, .pos = .{ .from = 0, .to = 0 } }, .allocator = allocator };
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

  pub const ParserError = error{
    UnexpectedToken,
    InvalidOperator,
    OutOfMemory,
    Overflow,
    InvalidCharacter,
  };  
  
  pub fn parse(self: *Parser, min_bp: u8) ParserError!Expression {
    
    self.consume(); // Consume the first token (likely an atom, but can be an operator too)
    // self.current now has that token
    // print token
    print("Parsing token: {s} at position {d}\n", .{ @tagName(self.current.tag), self.current.pos.from });

    var lhs: Expression =
      switch (self.current.tag) {
        .Integer => Expression{ .type = .Number, .value = .{ .i = try std.fmt.parseInt(i64, self.expr[self.current.pos.from..self.current.pos.to], 10) }, .pos = self.current.pos, .children = null },
        .Real => Expression{ .type = .Number, .value = .{ .f = try std.fmt.parseFloat(f64, self.expr[self.current.pos.from..self.current.pos.to]) }, .pos = self.current.pos, .children = null },
        .Variable => Expression{ .type = .Variable, .value = null, .pos = self.current.pos, .children = null },
        .Op => prefix: {
          const op_text: []const u8 = self.expr[self.current.pos.from..self.current.pos.to];
          const op_type: ?ExprType = getPrefixOperator(op_text);
          if (op_type == null) {
            return ParserError.InvalidOperator;
          }
          const r_bp = try prefixBindingPower(op_type);
          const expr = try self.parse(r_bp);
          const children = try self.allocator.alloc(Expression, 1);
          children[0] = expr;
          break :prefix Expression{ .type = op_type.?, .value = null, .pos = self.current.pos, .children = children.ptr };
        },
        else => return ParserError.UnexpectedToken,
      };
    
    print("Parsed lhs: {s} at position {d}\n", .{ @tagName(lhs.type), lhs.pos.from });

    while (true) {
      const op = self.peek();
      var implicit_mul: bool = false;
      switch (op.tag) {
        .Op => {}, // Allow these
        .Variable => {implicit_mul = true;}, // Implicit multiplication
        .Eof => break,
        else => return ParserError.UnexpectedToken,
      }

      // Convert TokenType to ExprType
      const op_text: []const u8 = self.expr[op.pos.from..op.pos.to];
      const op_type: ?ExprType = getInfixOperator(op_text, op.tag);
      const l_bp, const r_bp = try infixBindingPower(op_type);
      if (l_bp < min_bp) break;

      if (!implicit_mul) self.consume(); // Consume the operator token

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

pub fn main() !void {
    // Initialize the parsing rules

    var token_stream: TokenStream = TokenStream.init(Allocator);
    defer token_stream.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const expr: [:0]const u8 = "-a*bc^2-a^2";
    var tokenizer = Tokenizer.init(expr);
    print("-- start -- : {s}\n", .{expr});

    while (tokenizer.next()) |token| {
      if (token.tag == .Eof) {
        print("--eof--\n", .{});
        break;
      }
      try token_stream.append(token);
      tokenizer.dump(&token);
    }

    var parser = Parser.init(token_stream, expr, allocator);
    print("Token stream length: {d}\n", .{token_stream.items.len});
    const ast = try parser.parse(0);
    print("Expr: {}\n", .{ast});

    print("AST\n", .{});
    printAST(&ast, 0);
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

test "Complex expression with variables and functions" {
    try testTokenize("a_{1}+\\sin*3", &.{ .Variable, .Op, .FunctionName, .Op, .Integer });
}

test "Multiple character variables" {
    try testTokenize("abc_{123}", &.{ .Variable, .Variable, .Variable });
}

test "Complex mathematical expression" {
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

fn printAST(expr: *const Expression, indent: u32) void {
  const spaces = "                    ";
  const prefix = spaces[0..@min(indent, spaces.len)];

  switch (expr.type) {
    .Variable => {
      print("{s}{s}\n", .{ prefix, "Var"});
    },
    .Number => {
      if (expr.value) |val| {
        switch (val) {
          .i => |i| print("{s}Number: {d}\n", .{ prefix, i }),
          .f => |f| print("{s}Number: {d}\n", .{ prefix, f }),
        }
      } else {
        print("{s}Number: <no value>\n", .{prefix});
      }
    },
    .Add, .Sub, .Mul, .Div, .Dot, .Pow => {
      print("{s}{s}\n", .{ prefix, @tagName(expr.type) });
      if (expr.children) |children| {
        // print("{s}|-- Left:\n", .{prefix});
        printAST(&children[0], indent + 1);
        // print("{s}\\-- Right:\n", .{prefix});
        printAST(&children[1], indent + 1);
      } else {
        print("{s}\\-- <no children>\n", .{prefix});
      }
    },
    .iMul => {
      print("{s}{s}\n", .{ prefix, "Mul" });
      if (expr.children) |children| {
        // print("{s}|-- Left:\n", .{prefix});
        printAST(&children[0], indent + 1);
        // print("{s}\\-- Right:\n", .{prefix});
        printAST(&children[1], indent + 1);
      } else {
        print("{s}\\-- <no children>\n", .{prefix});
      }
    },
    .UnaryMinus, .UnaryPlus => {
      print("{s}{s}\n", .{ prefix, @tagName(expr.type) });
      if (expr.children) |children| {
        printAST(&children[0], indent + 1);
      } else {
        print("{s}\\-- <no children>\n", .{prefix});
      }
    },
    .Invalid => {
      print("{s}Invalid Expression\n", .{prefix});
    },
    // else => {
    //   @panic("unrecognized node");
    // }
  }
}