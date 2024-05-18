pub const Deserializer = struct {
    ptr: *anyopaque,
    deserializeFn: *const fn (ptr: *anyopaque) anyerror!void,

    fn deserialize(self: Deserializer) !void {
        return self.deserializeFn(self.ptr);
    }
};

//pub fn Deserializer(comptime O: type) type {
//    return struct {
//        const Self = @This();
//
//        deserializeFn: fn (self: *Self) ?O,
//
//        pub fn deserialize(self: *Self) ?O {
//            return self.deserializeFn(self);
//        }
//    };
//}
