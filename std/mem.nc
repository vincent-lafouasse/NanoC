// no simd yet

fn memcpy(dest: u8*, src: u8*, n: u32) {
    var i: u32 = 0;

    while (i < n) {
        dest[i] = src[i];
        i = i + 1;
    }
}

fn memcmp(a: u8*, b: u8*, n: u32) -> i32 {
{
    var i: u32 = 0;
    var diff: i32 = undefined;

    while (i < n)
    {
        diff = a[i] - b[i];
        if (diff) {
            return diff;
        }
        i = i + 1;
    }

    return 0;
}

fn memset(buffer: u8*, value: u8, n: u32) {
    var i: u32 = 0;

    while (i < n) {
        buffer[i] = value;
        i = i + 1;
    }
}
