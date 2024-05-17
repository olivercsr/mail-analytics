const std = @import("std");

pub fn JsonDeserializer(comptime T: type) type {
    return struct {
        const Self = @This();

        pub fn doDeserialize() ?T {
            return null;
        }
    };
}
