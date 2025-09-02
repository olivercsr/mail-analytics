const std = @import("std");
const deserializer = @import("../deserializer.zig");

pub const JsonDeserializer = struct {
    input: std.fs.File,

    fn deserialize(ptr: *anyopaque) !void {
        const self: *JsonDeserializer = @ptrCast(@alignCast(ptr));

        std.debug.print("deserialize {}", .{self});
        return null;
    }
};

//pub fn JsonDeserializer(comptime I: type, comptime O: type) type {
//    return struct {
//        const Self = @This();
//
//        input: I,
//
//        pub fn init(input: I) Self {
//            return Self{ .deserializer = deserializer.Deserializer(O){ .deserializeFn = doDeserialize }, .input = input };
//        }
//
//        pub fn doDeserialize(d: *deserializer.Deserializer(O)) ?O {
//            const self = @as(deserializer.Deserializer, @fieldParentPtr("deserializer", d));
//            return null;
//        }
//    };
//}
