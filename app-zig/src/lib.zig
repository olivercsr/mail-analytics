const std = @import("std");
const testing = std.testing;

const c = @cImport({
    @cDefine("_NO_CRT_STDIO_INLINE", "1");
    @cInclude("stdio.h");
});
const xml = @import("zig_xml");

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

export fn foo() void {
    _ = c.printf("hello from c\n");

    //const reader = xml.reader();
    //std.debug.print("well {n}", .{xml.ReaderOptions});
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}
