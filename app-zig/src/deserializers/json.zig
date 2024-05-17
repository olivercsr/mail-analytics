const std = @import("std");
const deserializer = @import("../deserializer.zig");

pub fn JsonDeserializer(comptime I: type, comptime O: type) type {
    return struct {
        const Self = @This();

        input: I,

        pub fn init(input: I) Self {
            return Self{ .deserializer = deserializer.Deserializer(O){ .deserializeFn = doDeserialize }, .input = input };
        }

        pub fn doDeserialize(d: *deserializer.Deserializer(O)) ?O {
            const self = @as(deserializer.Deserializer, @fieldParentPtr("deserializer", d));
            return null;
        }
    };
}
