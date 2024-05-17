pub fn Deserializer(comptime T: type) type {
    return struct {
        const Self = @This();

        deserializeFn: fn (self: *Self) ?T,

        pub fn deserialize(self: *Self) ?T {
            return self.deserializeFn(self);
        }
    };
}
