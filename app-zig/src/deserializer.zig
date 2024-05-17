pub fn Deserializer(comptime O: type) type {
    return struct {
        const Self = @This();

        deserializeFn: fn (self: *Self) ?O,

        pub fn deserialize(self: *Self) ?O {
            return self.deserializeFn(self);
        }
    };
}
