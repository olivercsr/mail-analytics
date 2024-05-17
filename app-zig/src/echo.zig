const std = @import("std");

pub fn echo(in: std.fs.File, out: std.fs.File) void {
    const reader = in.reader();
    var buffered_writer = std.io.bufferedWriter(out.writer());
    const writer = buffered_writer.writer();

    var buffer: [32]u8 = undefined;

    var len: usize = 1;
    while (len > 0) {
        len = reader.read(&buffer) catch |err| {
            std.debug.print("ERROR on read: {}\n", .{err});
            break;
        };
        _ = writer.write(buffer[0..len]) catch |err| {
            std.debug.print("ERROR on write: {}\n", .{err});
            break;
        };
    }
    buffered_writer.flush() catch |err| {
        std.debug.print("ERROR on flush: {}\n", .{err});
        return;
    };
}
