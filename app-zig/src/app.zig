const std = @import("std");
const echo = @import("echo.zig");
const formats = @import("formats.zig");
const formats_json = @import("formats/json.zig");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    try std.io.getStdErr().writer().print("and then some {s}\n", .{"dings"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!

    // ------------------
    const a = [_]u8{ 'a', 'b', 'c', 'd', 'e' };
    const a1 = a[0..3];
    std.debug.print("a1: {s}\n", .{a1});

    // ------------------
    echo.echo(std.io.getStdIn(), std.io.getStdOut());
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
