struct Point {
    x: i32,
    y: i32,
}

struct Node {
    value: u32,
    next: Node*,
}

var global_count: i32 = zeroed;
const MAX_SIZE: u32 = 100u;

var node: Node = zeroed;

/* TODO say hi */
// XXX hello
fn calculate(base: i32, offset: i32) {
    var result: i32;
    var point: Point;
    var node_ptr: Node*;

    result = base + offset;

    point.x = 10;
    point.y = 20;

    node_ptr->value = 42u;
    node_ptr->next = 0x0;

    var p: u8* = undefined;
    var pp: i32** = undefined;

    if result > 0 {
        return result;
    } else {
        return 0;
    }
}

fn main() {
cool:
    var x: i32 = calculate(10, 5);
    syscall(1, x, "hello world", '\n', '1', '\x67');

    // break and continue are not keywords
    const break: i32 = 67;
    var continue: i32 = break;
    goto cool;
}
