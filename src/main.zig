const std = @import("std");
const print = std.debug.print;

const tex = @import("tex");

const arg_parse = @import("arg_parse.zig");

pub fn main() !void {
    const gpa = std.heap.smp_allocator;

    var arena_impl: std.heap.ArenaAllocator = .init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var ip_buffer: [4096]u8 = undefined;
    var op_buffer: [4096]u8 = undefined;

    var token_stream: tex.TokenStream = .empty;
    defer token_stream.deinit(gpa);

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    const arg_parser = arg_parse.ArgParser.parse(args[1..]);

    std.debug.print("Output Format: {s}\n", .{@tagName(arg_parser.output_format)});

    var expr: [:0]u8 = undefined;
    if (arg_parser.input_file_path) |input_file_path| {
        const file = try std.fs.cwd().openFile(input_file_path, .{});
        defer file.close();
        var fr = file.reader(&ip_buffer);
        var aw: std.Io.Writer.Allocating = .init(gpa);
        _ = try fr.interface.streamRemaining(&aw.writer);
        expr = try aw.toOwnedSliceSentinel(0);
    } else {
        var fr = std.fs.File.stdin().reader(&ip_buffer);
        var aw: std.Io.Writer.Allocating = .init(gpa);
        _ = try fr.interface.streamRemaining(&aw.writer);
        expr = try aw.toOwnedSliceSentinel(0);
    }
    defer gpa.free(expr);

    var tokenizer = tex.Tokenizer.init(expr);

    while (tokenizer.next()) |token| {
        if (token.tag == .Eof) {
            print("--eof--\n", .{});
            break;
        }
        try token_stream.append(gpa, token);
        tokenizer.dump(&token);
    }

    var parser = tex.Parser.init(token_stream, expr, arena);
    print("Token stream length: {d}\n", .{token_stream.items.len});
    const ast = try parser.parse(0);
    print("input: {}\n", .{ast});

    const render: tex.RenderFunctionType = switch (arg_parser.output_format) {
        .ast => tex.renderAST,
        .polish => tex.renderPolish,
    };

    if (arg_parser.output_file_path) |output_file_path| {
        var file = try std.fs.cwd().createFile(output_file_path, .{});
        defer file.close();
        const writer_impl = file.writer(&op_buffer);
        var writer = writer_impl.interface;

        try render(&writer, &ast, expr);
        try writer.flush();
    } else {
        const stdout_writer = std.fs.File.stdout().writer(&op_buffer);
        var stdout = stdout_writer.interface;

        try render(&stdout, &ast, expr);
        try stdout.flush();
    }
}
