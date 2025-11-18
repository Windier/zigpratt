const std = @import("std");
const builtin = @import("builtin");

pub fn printDebug(comptime fmt: []const u8, args: anytype) void {
    if (builtin.target.os.tag == .freestanding) return;

    std.log.debug(fmt, args);
}
