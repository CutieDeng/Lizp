const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator; 

pub const Expression = union(enum) {
    pub const ListType = std.ArrayListUnmanaged(@This()); 

    pub const ParseError = error { ParseError } || Allocator.Error; 
    pub const ParseErrorDetail = struct { 
        line_number: isize, 
        column_number: isize, 
        details: union(enum) {
            MissingQuoted, 
            MissingRightParenthesis, 
            UnexpectedRightParenthesis, 
            UnsupportEscapeSlice: []const u8, 
            Unknown: []const u8, 
        }, 
    }; 

    Atom: []const u8, 
    List: ListType, 

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        switch (self.*) {
            .Atom => {}, 
            .List => {
                for (self.List.items) |*i| {
                    i.deinit(allocator); 
                }
                self.List.deinit(allocator); 
            }
        }
    }

    pub fn initParse(allocator: Allocator, contents: []const u8, error_info: *ParseErrorDetail) ParseError!@This() {
        var now_idx: usize = 0; 
        var exp : Expression = .{ .List = .{} }; 
        errdefer exp.deinit(allocator); 
        var ln: isize = 0; 
        var cn: isize = 0; 
        const rst = try initParseImpl(allocator, contents, &now_idx, error_info, &exp, &ln, &cn); 
        if (rst) {
            return exp; 
        } else {
            error_info.* = .{ 
                .line_number = ln, 
                .column_number = cn, 
                .details = .UnexpectedRightParenthesis, 
            };
            return error.ParseError; 
        }
    }

    const ParseState = union(enum) {
        Empty, 
        Quoted: struct { start: usize, is_escaped: bool }, 
        Normal: usize, 
        Commented, 
    }; 

    fn initParseImpl(
        allocator: Allocator, 
        contents: []const u8, 
        now_idx: *usize, 
        error_info: *ParseErrorDetail, 
        // just assume it as a 'List' 
        super: *Expression, 
        line: *isize, 
        col: *isize, 
    ) ParseError!bool { 
        var state: ParseState = .Empty; 
        var line_break: usize = 0; 
        while (true) {
            // handle the ending problem ~ 
            if (now_idx.* >= contents.len) {
                if (state == .Quoted) {
                    error_info.* = .{ 
                        .line_number = line.*, 
                        .column_number = col.*, 
                        .details = .MissingQuoted, 
                    };
                    return error.ParseError; 
                } 
                if (state == .Normal) {
                    // cool 
                    const p = state.Normal; 
                    const new = try super.List.addOne(allocator); 
                    new.* = .{ .Atom = contents[p..now_idx.*] }; 
                }
                return true; 
            }
            // normal start 
            const current_char_utf8 = contents[now_idx.*]; 
            const is_current_char_empty: bool = 
                false 
                or current_char_utf8 == ' '
                or current_char_utf8 == '\t'
                or current_char_utf8 == '\r'
                or current_char_utf8 == '\n'; 
            const is_squote = current_char_utf8 == '\''; 
            const is_quoted = current_char_utf8 == '"'; 
            const is_commented = current_char_utf8 == ';'; 
            const is_left_pr = current_char_utf8 == '('; 
            const is_right_pr = current_char_utf8 == ')'; 
            const is_line_break: bool = 
                if (current_char_utf8 == '\r') bk1: {
                    line_break = 1; 
                    break :bk1 true; 
                } 
                else if (current_char_utf8 == '\n') bk2: {
                    if (line_break == 1) {
                        line_break = 0;    
                        break :bk2 false; 
                    }
                    break :bk2 true; 
                } else bk3: {
                    line_break = 0; 
                    break :bk3 false; 
                }; 
            const ignore_col_move = 
                current_char_utf8 == '\r' 
                or current_char_utf8 == '\n'; 
            // pattern 1.1: empty input + empty state 
            if (is_current_char_empty and state == .Empty) {
            }
            // pattern 1.2: squote input + empty state 
            else if (is_squote and state == .Empty) {
                const new = try super.List.addOne(allocator); 
                new.* = .{ .Atom = "'" };
            } 
            // pattern 1.3: quoted input + empty state 
            else if (is_quoted and state == .Empty) {
                state = .{ .Quoted = .{ .start = now_idx.*, .is_escaped = false, } }; 
            }
            // pattern 1.4: comment input + empty state 
            else if (is_commented and state == .Empty) {
                state = .Commented; 
            }
            // pattern 1.5: left parenthesis input + empty state 
            else if (is_left_pr and state == .Empty) {
                var new_l: Expression = .{ .List = .{} }; 
                errdefer new_l.deinit(allocator); 
                state = .Empty; 
                now_idx.* += 1; col.* += 1;  
                // const not_meet_right_pr = try initParseImpl(allocator, contents, now_idx, now_idx, 
                const not_meet_right_pr = try initParseImpl(allocator, contents, now_idx, 
                    error_info, &new_l, line, col); 
                if (!not_meet_right_pr) {
                    std.debug.assert ( contents[now_idx.*] == ')' ); 
                } else {
                    error_info.* = .{ 
                        .line_number = line.*, 
                        .column_number = col.*, 
                        .details = .MissingRightParenthesis, 
                    }; 
                    return error.ParseError; 
                }
                const new = try super.List.addOne(allocator); 
                new.* = new_l; 
                now_idx.* += 1; 
                col.* += 1; 
                continue ; 
            } 
            // pattern 1.6: right parenthesis input + empty state 
            else if (is_right_pr and state == .Empty) {
                return false; 
            }
            // pattern 1.7: otherwise input + empty state 
            else if (state == .Empty) {
                state = .{ .Normal = now_idx.* }; 
            }
            // pattern 2.1: empty input + normal state 
            else if (is_current_char_empty and state == .Normal) {
                const p = state.Normal; 
                const new = try super.List.addOne(allocator); 
                new.* = .{ .Atom = contents[p..now_idx.*] }; 
                state = .Empty; 
            }
            // pattern 2.2: squote input + normal state 
            else if (is_squote and state == .Normal) {
                const p = state.Normal; 
                const new = try super.List.addOne(allocator); 
                new.* = .{ .Atom = contents[p..now_idx.*] }; 
                const new2 = try super.List.addOne(allocator); 
                new2.* = .{ .Atom = "'" }; 
                state = .Empty; 
            }
            // pattern 2.3: quoted input + normal state 
            else if (is_quoted and state == .Normal) {
                const p = state.Normal; 
                const new = try super.List.addOne(allocator); 
                new.* = .{ .Atom = contents[p..now_idx.*] }; 
                state = .{ .Quoted = .{ .start = now_idx.*, .is_escaped = false, } }; 
            }
            // pattern 2.4: comment input + normal state 
            else if (is_commented and state == .Normal) {
                const p = state.Normal; 
                const new = try super.List.addOne(allocator); 
                new.* = .{ .Atom = contents[p..now_idx.*] }; 
                state = .Commented; 
            }
            // pattern 2.5: left parthenesis input + normal state 
            else if (is_left_pr and state == .Normal) {
                const p = state.Normal; 
                const new = try super.List.addOne(allocator); 
                new.* = .{ .Atom = contents[p..now_idx.*] }; 
                var new_l: Expression = .{ .List = .{} }; 
                errdefer new_l.deinit(allocator); 
                state = .Empty; 
                now_idx.* += 1; col.* += 1; 
                const not_meet_right_pr = try initParseImpl(allocator, contents, now_idx, 
                    error_info, &new_l, line, col); 
                if (!not_meet_right_pr) {
                    std.debug.assert ( contents[now_idx.*] == ')' ); 
                } else {
                    error_info.* = .{ 
                        .line_number = line.*, 
                        .column_number = col.*, 
                        .details = .MissingRightParenthesis, 
                    }; 
                    return error.ParseError; 
                }
                const new2 = try super.List.addOne(allocator); 
                new2.* = new_l; 
                now_idx.* += 1; 
                col.* += 1; 
                continue ; 
            }
            // pattern 2.6: right parenthesis input + normal state 
            else if (is_right_pr and state == .Normal) {
                const p = state.Normal; 
                const new = try super.List.addOne(allocator); 
                new.* = .{ .Atom = contents[p..now_idx.*] }; 
                return false; 
            }
            // pattern 2.7: otherwise input + normal state 
            else if (state == .Normal) {
                // ignore ... 
            }
            // pattern 3.1: linebreak and commented state 
            else if (is_line_break and state == .Commented) {
                // 
                state = .Empty; 
            }
            // pattern 3.2: anything else and commented state 
            else if (state == .Commented) {
            }
            // pattern 4.1: quoted input + quoted state 
            else if (is_quoted and state == .Quoted) {
                if (state.Quoted.is_escaped) {
                    // ignore it properly 
                    state.Quoted.is_escaped = false; 
                } else {
                    const p = state.Quoted.start; 
                    const new = try super.List.addOne(allocator); 
                    new.* = .{ .Atom = contents[p..now_idx.*] }; 
                    state = .Empty; 
                }
            }
            // pattern 4.2: escape input + quoted state 
            else if (current_char_utf8 == '\\' and state == .Quoted) {
                state.Quoted.is_escaped = !state.Quoted.is_escaped; 
            }
            // pattern 4.3: anything + quoted state 
            else if (state == .Quoted) {
                // check escaped or not ... 
                if (state.Quoted.is_escaped) {
                    error_info.* = .{ 
                        .line_number = line.*, 
                        .column_number = col.*, 
                        .details = .{ .UnsupportEscapeSlice = contents[now_idx.*-1..now_idx.*+1] },  
                    }; 
                    return error.ParseError; 
                }
            } 
            // pattern 5: never 
            else {
                unreachable; 
            }
            now_idx.* += 1; 
            if (is_line_break) {
                col.* = 0; 
                line.* += 1; 
            } else if (!ignore_col_move) {
                col.* += 1; 
            }
        }
    }

}; 

fn logExpression(exp: Expression, w: anytype, indent: usize) !void {
    switch(exp) {
        .Atom => |a| {
            for (0..indent) |_| {
                try w.print(" ", .{}); 
            }
            if (a[0] == '\'') {
                try w.print("QUOTED\n", .{});     
            } else if (a[0] == '"') {
                try w.print("\"...\"\n", .{}); 
            } else {
                try w.print("SYM {s}\n", .{ a }); 
            }
        }, 
        .List => |l| {
            for (l.items) |i| {
                try logExpression(i, w, indent + 2); 
            }
            if (l.items.len == 0) {
                for (0..indent+2) |_| {
                    try w.print(" ", .{}); 
                }
                try w.print("EMPTY LIST\n", .{});
            }
        }
    }
}

test {
    const input = "hello world()\"\\\\[\"kgf"; 
    const a = testing.allocator;
    var e: Expression.ParseErrorDetail = undefined; 
    var f = Expression.initParse(a, input,  &e) 
        catch |err| switch (err) {
            error.ParseError => {
                std.log.warn("failed;\n{any}", .{ e }); 
                return ; 
            }, 
            else => unreachable, 
        };
    defer f.deinit(a);
    const stdout = std.io.getStdErr().writer(); 
    try logExpression(f, stdout, 0); 
}