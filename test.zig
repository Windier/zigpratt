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
            .operator_name => {
              
              switch (self.buffer[self.index]) {
                 '{' => {
                  self.index += 1;
                  result.pos.from = self.index; // ignore the opening brace
                  continue :state .operator_name;
                },
                'a'...'z', 'A'...'Z' => {self.index += 1; continue :state .operator_name;},
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
                            print("Keyword found: {s} -> {s}\n", .{text, @tagName(tag)});
                            if (tag == .OperatorName){result.pos.from = self.index; continue :state .operator_name;}

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
    Juxt,
    Comma,
    With,
    FunctionCall,
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
  .{ "^", .Pow }, // iMul is used for implicit multiplication, e.g., ab is a iMul b
  .{ ".", .Dot },
  .{ "with", .With },
});

pub const prefix_operators = std.StaticStringMap(ExprType).initComptime(.{
  .{ "+", .UnaryPlus },
  .{ "-", .UnaryMinus },
});

pub fn get_infix_operator(text: []const u8, tag: TokenType) ?ExprType {
    if (tag == .Variable) return .iMul;
    return infix_operators.get(text);
}

pub fn get_prefix_operator(text: []const u8) ?ExprType {
    return prefix_operators.get(text);
}


const Expression = struct {
    type: ExprType,
    value: ?union(enum) { i: i64, f: f64, length: u64 },
    pos: Loc,
    children: ?[*]Expression, // Might be null for literals
};

fn infix_binding_power(op: ?ExprType) error{InvalidOperator}!struct { i8, i8 } {
  if (op == null) return error.InvalidOperator;
  switch (op.?) {
    .Comma => return .{ -1, -1 },
    .Add, .Sub => return .{ 1, 2 },
    .Mul, .Div => return .{ 3, 4 },
    .iMul => return .{ 3, 4 },
    .Dot => return .{ 6, 5 },
    .Pow => return .{ 7, 6 },
    .With => return .{ 8, 7 },
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
    print("Current token: {s} text: {s}\n", .{ @tagName(self.current.tag), self.expr[self.current.pos.from..self.current.pos.to] });
    self.head += 1;
  }

  pub fn peek(self: *Parser) _Token {
    if (self.head >= self.token_stream.items.len) {
      return .{ .tag = .Eof, .pos = .{ .from = 0, .to = 0 } };
    }
    print("Peeking token: {s} text: {s}\n", .{ @tagName(self.token_stream.items[self.head].tag), self.expr[self.token_stream.items[self.head].pos.from..self.token_stream.items[self.head].pos.to] });
    return self.token_stream.items[self.head];
  }

  pub const ParserError = error{
    UnexpectedToken,
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
    
    const children = try self.allocator.alloc(Expression, 1); // Allocate memory for the children array
    children[0] = expr;

    return Expression{ .type = op_type.?, .value = null, .pos = op.pos, .children = children.ptr };
  }

  // pub fn parse_object(self: *Parser, len: u32) ParserError!Expression {

  //   var children = try self.allocator.alloc(Expression, len);
  //   for (0..len) |i| {
  //     children[i] = try self.parse(0);
  //     // self.consume(); // consume the comma
  //   }

  //   return Expression{ .type = .Object, .value = null, .pos = self.current.pos, .children = children.ptr };

  // }

// ((x,y,z), y)
  pub fn parse_paren(self: *Parser) ParserError!Expression {
    // Paren can be for grouping (has .Comma) or simply to wrap an expression

    // Lookahead for commas
    var level: i32 = 0;
    var commas: u32 = 0;
    for (self.token_stream.items[self.head..]) |token| {
      // print("{s}\n", .{@tagName(token.tag)});
      switch (token.tag) {
        .LParen =>  { level -= 1; },
        .RParen =>  { if (level == 0) break; level += 1; },
        .Comma =>  { if (level == 0) commas += 1; },
        else => {},
      }
    }

    if (level != 0) return ParserError.UnmatchedParentheses;
    if (commas == 0) return try self.parse(0); // If there are no commas, it's a parenthesized expression

    const len = commas + 1;
    var children = try self.allocator.alloc(Expression, len);
    for (0..len) |i| {
      children[i] = try self.parse(0);
      if (i < len - 1) self.consume(); // consume the comma
    }

    return Expression{ .type = .Object, .value = .{ .length = len }, .pos = self.current.pos, .children = children.ptr };

  }

  
  pub fn parse(self: *Parser, min_bp: i8) ParserError!Expression {
    
    self.consume(); // Consume the first token (likely an atom, but can be an operator too)
    // self.current now has that token
    
    var lhs: Expression =
      switch (self.current.tag) {
        .Integer => Expression{ .type = .Number, .value = .{ .i = try std.fmt.parseInt(i64, self.expr[self.current.pos.from..self.current.pos.to], 10) }, .pos = self.current.pos, .children = null },
        .Real => Expression{ .type = .Number, .value = .{ .f = try std.fmt.parseFloat(f64, self.expr[self.current.pos.from..self.current.pos.to]) }, .pos = self.current.pos, .children = null },
        .Variable => Expression{ .type = .Variable, .value = null, .pos = self.current.pos, .children = null },
        .Op => try self.parse_prefix(),
        .LParen => paren: {
          const expr: Expression = try self.parse_paren(); // Parse the expression inside parentheses
          self.consume(); // Consume the ')' token
          if (self.current.tag != .RParen) return ParserError.UnexpectedToken; // Expect a closing parenthesis
          break :paren expr;
        },
        
        else => return ParserError.UnexpectedToken,
      };

    // print("Parsed lhs: {s} text: {s}\n", .{ @tagName(lhs.type), self.expr[lhs.pos.from..lhs.pos.to] });

    while (true) {
      const op = self.peek();
      var implicit_mul: bool = false;
      switch (op.tag) {
        .Op, .With => {}, // Allow these
        .Variable => { implicit_mul = true; }, // Implicit multiplication
        .Eof => break,
        .LParen => {},
        .Comma => break, // Comma returns the current expression
        .RParen => break, // Stop parsing on closing parenthesis
        else => return ParserError.UnexpectedToken,
      }

      // Convert TokenType to ExprType
      const op_text: []const u8 = self.expr[op.pos.from..op.pos.to];
      const op_type: ?ExprType = get_infix_operator(op_text, op.tag);
      const l_bp, const r_bp = try infix_binding_power(op_type);
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

    const expr: [:0]const u8 = "a\\operatorname{with}b"; // Example expression to parse
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

fn printAST(expr: *const Expression, _: u32) void {
  printASTHelper(expr, "", true);
}

fn printASTHelper(expr: *const Expression, prefix: []const u8, is_last: bool) void {
  // Print current node with appropriate connector
  const connector = if (is_last) "+-- " else "|-- ";
  
  switch (expr.type) {
    .Variable => {
      print("{s}{s}Var\n", .{ prefix, connector });
    },
    .Number => {
      if (expr.value) |val| {
        switch (val) {
          .i => |i| print("{s}{s}Number: {d}\n", .{ prefix, connector, i }),
          .f => |f| print("{s}{s}Number: {d}\n", .{ prefix, connector, f }),
          else => unreachable,
        }
      } else {
        print("{s}{s}Number: <no value>\n", .{ prefix, connector });
      }
    },
    .Add, .Sub, .Mul, .Div, .Dot, .Pow, .Juxt, .Comma, .With => {
      print("{s}{s}{s}\n", .{ prefix, connector, @tagName(expr.type) });
      if (expr.children) |children| {
        // Create new prefix: extend current with either spaces or vertical bar
        var new_prefix: [256]u8 = undefined;
        const extension = if (is_last) "    " else "|   ";
        const new_len = @min(prefix.len + 4, 252); // Leave room for extension
        @memcpy(new_prefix[0..prefix.len], prefix);
        @memcpy(new_prefix[prefix.len..new_len], extension);
        
        printASTHelper(&children[0], new_prefix[0..new_len], false);
        printASTHelper(&children[1], new_prefix[0..new_len], true);
      }
    },
    .Object => {
      print("{s}{s}Object\n", .{ prefix, connector });
      if (expr.children) |children| {
        var new_prefix: [256]u8 = undefined;
        const extension = if (is_last) "    " else "|   ";
        const new_len = @min(prefix.len + 4, 252);
        @memcpy(new_prefix[0..prefix.len], prefix);
        @memcpy(new_prefix[prefix.len..new_len], extension);
        
        const length = expr.value.?.length;
        for (0..length) |i| {
          const is_last_child = (i == length - 1);
          printASTHelper(&children[i], new_prefix[0..new_len], is_last_child);
        }
      }
    },
    .iMul, .FunctionCall => {
      print("{s}{s}Mul\n", .{ prefix, connector });
      if (expr.children) |children| {
        var new_prefix: [256]u8 = undefined;
        const extension = if (is_last) "    " else "|   ";
        const new_len = @min(prefix.len + 4, 252);
        @memcpy(new_prefix[0..prefix.len], prefix);
        @memcpy(new_prefix[prefix.len..new_len], extension);
        
        printASTHelper(&children[0], new_prefix[0..new_len], false);
        printASTHelper(&children[1], new_prefix[0..new_len], true);
      }
    },
    .UnaryMinus, .UnaryPlus => {
      print("{s}{s}{s}\n", .{ prefix, connector, @tagName(expr.type) });
      if (expr.children) |children| {
        var new_prefix: [256]u8 = undefined;
        const extension = if (is_last) "    " else "|   ";
        const new_len = @min(prefix.len + 4, 252);
        @memcpy(new_prefix[0..prefix.len], prefix);
        @memcpy(new_prefix[prefix.len..new_len], extension);
        
        printASTHelper(&children[0], new_prefix[0..new_len], true);
      }
    },
    .Invalid => {
      print("{s}{s}Invalid Expression\n", .{ prefix, connector });
    },
  }
}