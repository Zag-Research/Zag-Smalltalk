const stdout = @import("std").io.getStdOut().writer();

const n = 0x100000000;

pub fn main() void {
    stdout.print(
        "largest prime less than {} is {}\n",.{n,maxPrime()}
    ) catch unreachable;
}

fn maxPrime() usize {
    const allocator = @import("std").heap.page_allocator;

    const bits = allocator.alloc(bool, n) catch @panic("Failed Allocating");
    defer allocator.free(bits);

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
    
    i=n-1;
    while (!bits[i]) : (i-=1) {}
    return i;
}
