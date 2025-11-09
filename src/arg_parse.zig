const std = @import("std");

const fatal = @import("fatal.zig");

pub const OutputFormat = enum {
    ast,
    mathml,
};

pub const ArgParser = struct {
    input_file_path: ?[]const u8,
    output_file_path: ?[]const u8,
    output_format: OutputFormat,

    pub fn parse(args: []const []const u8) ArgParser {
        var input_file_path: ?[]const u8 = null;
        var output_file_path: ?[]const u8 = null;
        var output_format: OutputFormat = .mathml;

        const eql = std.mem.eql;
        const startsWith = std.mem.startsWith;

        var idx: usize = 0;
        while (idx < args.len) : (idx += 1) {
            const arg = args[idx];
            if (eql(u8, arg, "-h") or eql(u8, arg, "--help")) {
                fatal.help();
            } else if (eql(u8, arg, "-i") or eql(u8, arg, "--input")) {
                idx += 1;
                if (idx >= args.len) fatal.msg(
                    "error: missing argument to '{s}'",
                    .{arg},
                );
                input_file_path = args[idx];
            } else if (startsWith(u8, arg, "--input=")) {
                input_file_path = arg["--input=".len..];
            } else if (eql(u8, arg, "-o") or eql(u8, arg, "--output")) {
                idx += 1;
                if (idx >= args.len) fatal.msg(
                    "error: missing argument to '{s}'",
                    .{arg},
                );
                output_file_path = args[idx];
            } else if (startsWith(u8, arg, "--output=")) {
                output_file_path = arg["--output=".len..];
            } else if (eql(u8, arg, "-f") or eql(u8, arg, "--format")) {
                idx += 1;
                if (idx >= args.len) fatal.msg(
                    "error: missing argument to '{s}'",
                    .{arg},
                );
                const format_arg = args[idx];
                output_format = std.meta.stringToEnum(OutputFormat, format_arg) orelse {
                    fatal.msg("error: unexpected format '{s}'\n", .{format_arg});
                };
            } else if (startsWith(u8, arg, "--format=")) {
                output_format = std.meta.stringToEnum(OutputFormat, arg["--format=".len..]) orelse {
                    fatal.msg("error: unexpected format '{s}'\n", .{arg["--format=".len..]});
                };
            } else {
                fatal.msg("error: unexpected cli argument '{s}'\n", .{arg});
            }
        }

        return .{
            .input_file_path = input_file_path,
            .output_file_path = output_file_path,
            .output_format = output_format,
        };
    }
};
