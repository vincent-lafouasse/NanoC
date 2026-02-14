struct Point {
    x: i32,
    y: i32,
}

struct Node {
    value: u32,
    next: Node*,
}

var global_count: i32 = 0;
const MAX_SIZE: u32 = 100u;

fn calculate(base: i32, offset: i32) {
    var result: i32;
    var point: Point;
    var node_ptr: Node*;

    result = base + offset;

    point.x = 10;
    point.y = 20;

    node_ptr->value = 42u;
    node_ptr->next = 0x0;

    var p: u8*;
    var pp: i32**;

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
    goto cool;
}
