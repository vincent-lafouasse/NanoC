// NanoC Basics - Variables, arithmetic, and return values

// Global constants
const MAGIC_NUMBER: i32 = 42;
const MAX_VALUE: i32 = 100;

// Global variable (zero-initialized)
var global_count: i32;

fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn subtract(a: i32, b: i32) -> i32 {
    return a - b;
}

fn multiply(a: i32, b: i32) -> i32 {
    return a * b;
}

fn divide(a: i32, b: i32) -> i32 {
    return a / b;
}

fn modulo(a: i32, b: i32) -> i32 {
    return a % b;
}

fn main() -> i32 {
    // Local variables
    var x: i32 = 10;
    var y: i32 = 20;

    // Undefined variable (dangerous!)
    var uninitialized: i32;

    // Const local (must be initialized)
    const result: i32 = add(x, y);

    // Arithmetic operations
    var sum: i32 = x + y;              // 30
    var diff: i32 = y - x;             // 10
    var product: i32 = x * y;          // 200
    var quotient: i32 = y / x;         // 2
    var remainder: i32 = y % x;        // 0

    // Unary minus
    var negative: i32 = -x;            // -10

    // Comparisons
    if (x < y) {
        sum = sum + 1;
    }

    if (x == 10 && y == 20) {
        product = product * 2;
    }

    // Use global
    global_count = global_count + 1;

    return 0;
}
