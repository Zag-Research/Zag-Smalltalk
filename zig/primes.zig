const stdout = @import("std").io.getStdOut().writer();
//const expect = @import("std").testing.expect;

const n = 0x10000;

var bits : []bool = undefined;

pub fn main() void {
    var gpa = @import("std").heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) @panic("Allocation leakage");
    }
//    const allocator = @import("std").heap.page_allocator;
    bits = allocator.alloc(bool, n) catch @panic("Failed Allocating");
    defer allocator.free(bits);

    init_bits();
    const max = 0x100000000;
    var i : usize = max;
    while (i > max-1000) : (i -= 59) {
        i = largestPrimeLessThan(i);
        stdout.print("next largest trial prime is {}\n",.{i}) catch unreachable;
    }
}

fn init_bits() void {
    var i : usize = 3;
    while (i<n) : (i += 2) {
        bits[i-1]=false;
        bits[i]=true;
    }
    bits[0]=false;
    bits[1]=false;
    bits[2]=true;

    i = 3;
    while (i<n) : (i +=2) {
        if (bits[i]) {
            var j : usize = i+i;
            while (j<n) : (j += i) {
                bits[j]=false;
            }
        }
    }
}
fn isPrime(p : usize) bool {
    if (p<bits.len) return bits[p];
    if (p%2==0) return false;
    var i : usize = 3;
    while (i*i<p) : (i += 2) {
        if (bits[i]) {
            if (p%i==0) return false;
        }
    }
    return true;
}
fn largestPrimeLessThan(max : usize) usize {
    if (max<=3) return max-1;
    var i=(max&~@as(usize,1))-1;
    while (!isPrime(i)) : (i-=2) {}
    return i;
}
