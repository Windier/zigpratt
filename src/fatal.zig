const std = @import("std");
const builtin = @import("builtin");

pub fn msg(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print(fmt, args);
    if (builtin.mode == .Debug) std.debug.panic("\n\n(zemml debug stack trace)\n", .{});
    std.process.exit(1);
}

pub fn oom() noreturn {
    msg("oom\n", .{});
}

pub fn dir(path: []const u8, err: anyerror) noreturn {
    msg("error accessing dir '{s}': {s}\n", .{
        path, @errorName(err),
    });
}

pub fn file(path: []const u8, err: anyerror) noreturn {
    msg("error accessing file '{s}': {s}\n", .{
        path, @errorName(err),
    });
}

pub fn help() noreturn {
    std.debug.print(
        \\Usage: zemml [OPTIONS]
        \\
        \\ Options:
        \\  --input, -i       Input file path (default: stdin)
        \\  --output, -o      Output file path (default: stdout)
        \\  --format, -f      Output format [ast|mathml] (default: mathml)
        \\  --help, -h        Print this help message
        \\
    , .{});
    std.process.exit(0);
}
