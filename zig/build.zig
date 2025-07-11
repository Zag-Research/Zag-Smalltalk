const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const test_step = b.step("test", "Run LLVM tests");

    // LLVM MODULE
    const llvm_module = b.addModule("llvm", .{
        .root_source_file = b.path("./zag/libs/zig-llvm/src/llvm.zig"),
        .target = target,
        .optimize = optimize,
    });

    llvm_module.addCMacro("_FILE_OFFSET_BITS", "64");
    llvm_module.addCMacro("__STDC_CONSTANT_MACROS", "");
    llvm_module.addCMacro("__STDC_FORMAT_MACROS", "");
    llvm_module.addCMacro("__STDC_LIMIT_MACROS", "");
    llvm_module.linkSystemLibrary("z", .{});

    if (target.result.abi != .msvc)
        llvm_module.link_libc = true
    else
        llvm_module.link_libcpp = true;

    switch (target.result.os.tag) {
        .linux => llvm_module.linkSystemLibrary("LLVM-18", .{}), // Ubuntu
        .macos => {
            llvm_module.addLibraryPath(.{
                .cwd_relative = "/opt/homebrew/opt/llvm/lib",
            });
            llvm_module.linkSystemLibrary("LLVM", .{
                .use_pkg_config = .no,
            });
        },
        else => llvm_module.linkSystemLibrary("LLVM", .{
            .use_pkg_config = .no,
        }),
    }

    // CLANG MODULE
    const clang_module = b.addModule("clang", .{
        .root_source_file = b.path("./zag/libs/zig-llvm/src/clang.zig"),
        .target = target,
        .optimize = optimize,
    });

    if (target.result.abi != .msvc)
        clang_module.link_libc = true
    else
        clang_module.link_libcpp = true;

    switch (target.result.os.tag) {
        .linux => clang_module.linkSystemLibrary("clang-18", .{}), // Ubuntu
        .macos => {
            clang_module.addLibraryPath(.{
                .cwd_relative = "/opt/homebrew/opt/llvm/lib",
            });
            clang_module.linkSystemLibrary("clang", .{
                .use_pkg_config = .no,
            });
        },
        else => clang_module.linkSystemLibrary("clang", .{
            .use_pkg_config = .no,
        }),
    }

    // Add a new option for specifying the LLVM path
    const llvm_file_path = b.option([]const u8, "llvm-path", "Path to the LLVM file") orelse "";
    // Build executable
    if (llvm_file_path.len > 0) {
        buildLLVMFile(b, .{
            .filepath = llvm_file_path,
            .target = target,
            .optimize = .Debug,
        });
    }

    // Build tests
    const llvm_test_file_path = b.option([]const u8, "llvm-test-path", "Path to the LLVM file that contains tests") orelse "";
    if (llvm_test_file_path.len > 0) {
        buildLLVMTestFile(b, .{
            .filepath = llvm_test_file_path,
            .target = target,
            .optimize = .Debug,
        }, test_step);
    }
}

fn buildLLVMFile(b: *std.Build, i: BuildInfo) void {
    const exe = b.addExecutable(.{
        .name = i.filename(),
        .root_source_file = b.path(i.filepath),
        .target = i.target,
        .optimize = i.optimize,
    });

    exe.root_module.addImport("llvm", b.modules.get("llvm").?);
    // Install the executable into zig-out/bin
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step(i.filename(), b.fmt("Run the {s}", .{i.filename()}));
    run_step.dependOn(&run_cmd.step);
}

fn buildLLVMTestFile(b: *std.Build, i: BuildInfo, test_step: *std.Build.Step) void {
    std.debug.print(">> building test file: {s}\n", .{i.filepath});
    const test_exe = b.addTest(.{
        .name = i.filename(),
        .root_source_file = b.path(i.filepath),
        .target = i.target,
        .optimize = i.optimize,
    });

    test_exe.root_module.addImport("llvm", b.modules.get("llvm").?);

    const run_cmd = b.addRunArtifact(test_exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    test_step.dependOn(&run_cmd.step);
}

const BuildInfo = struct {
    filepath: []const u8,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,

    fn filename(self: BuildInfo) []const u8 {
        var split = std.mem.splitSequence(u8, std.fs.path.basename(self.filepath), ".");
        return split.first();
    }
};
