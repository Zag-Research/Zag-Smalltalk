const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create a "update-submodules" step
    const update_submodules_step = b.step("update-submodules", "Update git submodules");
    const run_step = b.addSystemCommand(&[_][]const u8{ "git", "submodule", "update", "--init", "--recursive" });
    update_submodules_step.dependOn(&run_step.step);

    // LLVM MODULE
    const llvm_module = b.addModule("llvm", .{
        .root_source_file = b.path("./libs/zig-llvm/src/llvm.zig"),
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
        .root_source_file = b.path("./libs/zig-llvm/src/clang.zig"),
        .target = target,
        .optimize = optimize,
    });
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

    if (target.result.abi != .msvc)
        clang_module.link_libc = true
    else
        clang_module.link_libcpp = true;

    // Instead of building examples from examples/, we now build a file from llvm/ directory
    // If you want to conditionally build a specific llvm file, add logic here.
    const examples = b.option(bool, "Examples", "Build all examples [default: false]") orelse false;
    if (examples) {
        buildExample(b, .{
            .filepath = "./llvm/example.zig",
            .target = target,
            .optimize = .Debug,
        });
    }

    buildTests(b, target);
}

fn buildExample(b: *std.Build, i: BuildInfo) void {
    const exe = b.addExecutable(.{
        .name = i.filename(),
        .root_source_file = b.path(i.filepath),
        .target = i.target,
        .optimize = i.optimize,
    });
    exe.root_module.addImport("llvm", b.modules.get("llvm").?);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step(i.filename(), b.fmt("Run the {s}", .{i.filename()}));
    run_step.dependOn(&run_cmd.step);
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

fn buildTests(b: *std.Build, target: std.Build.ResolvedTarget) void {
    const llvm_tests = b.addTest(.{
        .root_source_file = b.path("./libs/zig-llvm/src/llvm.zig"),
        .target = target,
        .optimize = .Debug,
        .name = "llvm-tests",
    });
    const clang_tests = b.addTest(.{
        .root_source_file = b.path("./libs/zig-llvm/src/clang.zig"),
        .target = target,
        .optimize = .Debug,
        .name = "clang-tests",
    });
    llvm_tests.root_module.addImport("llvm", b.modules.get("llvm").?);
    clang_tests.root_module.addImport("clang", b.modules.get("clang").?);

    const run_llvm_tests = b.addRunArtifact(llvm_tests);
    const test_llvm_step = b.step("test", "Run LLVM-binding tests");
    test_llvm_step.dependOn(&run_llvm_tests.step);
}
