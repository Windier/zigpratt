
const expr_type = enum {
  Number,
  Variable,
  Add,
  Sub,
  Mul,
  Div,
  List,
  Negate,
  Object
};

const Expression = union(enum) {
    Number: f64,
    Variable: []const u8,
    Unary: struct {
        op: TokenType,
        right: *Expression,
    },
    Binary: struct {
        left: *Expression,
        op: TokenType,
        right: *Expression,
    },
    Grouping: struct {
        expression: *Expression,
    },

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Unary => |u| u.right.deinit(allocator),
            .Binary => |b| {
                b.left.deinit(allocator);
                b.right.deinit(allocator);
            },
            .Grouping => |g| g.expression.deinit(allocator),
            else => {},
        }
        allocator.destroy(self);
    }
};

const Parser = struct {
    tokenizer: Tokenizer,
    allocator: std.mem.Allocator,
    previous: _Token,
    current: _Token,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: [:0]const u8) Parser {
        return .{
            .tokenizer = Tokenizer.init(source),
            .allocator = allocator,
            .previous = undefined,
            .current = undefined,
        };
    }

    fn advance(self: *Self) void {
        self.previous = self.current;
        self.current = self.tokenizer.next();
        // TODO: Handle error tokens
    }

    fn consume(self: *Self, expected_tag: TokenType) !void {
        if (self.current.tag == expected_tag) {
            self.advance();
            return;
        }
        return error.UnexpectedToken;
    }

    fn getRule(tag: TokenType) *const ParseRule {
        return &rules[@intFromEnum(tag)];
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) !*Expression {
        self.advance();
        const prefix_rule = self.getRule(self.previous.tag).prefix;

        if (prefix_rule == null) {
            return error.ExpectedExpression;
        }

        var left_expression = try prefix_rule.?(self);

        while (precedence.numeric() <= self.getRule(self.current.tag).precedence.numeric()) {
            self.advance();
            const infix_rule = self.getRule(self.previous.tag).infix;
            if (infix_rule == null) return error.UnexpectedToken;
            left_expression = try infix_rule.?(self, left_expression);
        }

        return left_expression;
    }

    fn number(self: *Self) !*Expression {
        const value = std.fmt.parseFloat(f64, self.tokenizer.buffer[self.previous.pos.from..self.previous.pos.to]) catch return error.InvalidNumber;
        const expr = try self.allocator.create(Expression);
        expr.* = .{ .Number = value };
        return expr;
    }

    fn variable(self: *Self) !*Expression {
        const name = self.tokenizer.buffer[self.previous.pos.from..self.previous.pos.to];
        const expr = try self.allocator.create(Expression);
        expr.* = .{ .Variable = name };
        return expr;
    }

    fn grouping(self: *Self) !*Expression {
        const expression = try self.parsePrecedence(.Assignment);
        try self.consume(TokenType.RParen);
        const expr = try self.allocator.create(Expression);
        expr.* = .{ .Grouping = .{ .expression = expression } };
        return expr;
    }

    fn unary(self: *Self) !*Expression {
        const op_type = self.previous.tag;
        const right = try self.parsePrecedence(.Unary);
        const expr = try self.allocator.create(Expression);
        expr.* = .{ .Unary = .{ .op = op_type, .right = right } };
        return expr;
    }

    fn binary(self: *Self, left: *Expression) !*Expression {
        const op_type = self.previous.tag;
        const rule = self.getRule(op_type);
        const right = try self.parsePrecedence(rule.precedence.next());
        const expr = try self.allocator.create(Expression);
        expr.* = .{ .Binary = .{ .left = left, .op = op_type, .right = right } };
        return expr;
    }

    pub fn parse(self: *Self) !*Expression {
        self.advance();
        const expression = self.parsePrecedence(.Assignment);
        try self.consume(TokenType.Eof);
        return expression;
    }
};

const Precedence = enum(u8) {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,

    pub fn numeric(self: Precedence) u8 {
        return @intFromEnum(self);
    }

    pub fn next(self: Precedence) Precedence {
        const next_val = @intFromEnum(self) + 1;
        if (next_val > @typeInfo(Precedence).Enum.fields.len - 1) {
            return .Primary;
        }
        return @enumFromInt(next_val);
    }
};

const ParseFn = *const fn (parser: *Parser.Self) anyerror!*Expression;
const InfixParseFn = *const fn (parser: *Parser.Self, left: *Expression) anyerror!*Expression;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?InfixParseFn,
    precedence: Precedence,
};

var rules: [_]ParseRule = undefined;

fn buildRules() void {
    rules = .{
        [_]ParseRule{ .prefix = null, .infix = null, .precedence = .None } ** (@typeInfo(TokenType).Enum.fields.len),
    };
    rules[@intFromEnum(TokenType.LParen)] = .{ .prefix = Parser.grouping, .infix = null, .precedence = .None };
    rules[@intFromEnum(TokenType.Minus)] = .{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .Term };
    rules[@intFromEnum(TokenType.Plus)] = .{ .prefix = null, .infix = Parser.binary, .precedence = .Term };
    rules[@intFromEnum(TokenType.Slash)] = .{ .prefix = null, .infix = Parser.binary, .precedence = .Factor };
    rules[@intFromEnum(TokenType.Asterisk)] = .{ .prefix = null, .infix = Parser.binary, .precedence = .Factor };
    rules[@intFromEnum(TokenType.Integer)] = .{ .prefix = Parser.number, .infix = null, .precedence = .None };
    rules[@intFromEnum(TokenType.Real)] = .{ .prefix = Parser.number, .infix = null, .precedence = .None };
    rules[@intFromEnum(TokenType.Variable)] = .{ .prefix = Parser.variable, .infix = null, .precedence = .None };
    rules[@intFromEnum(TokenType.BinaryOp)] = .{ .prefix = null, .infix = Parser.binary, .precedence = .Term };
}
