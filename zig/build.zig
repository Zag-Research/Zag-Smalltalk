const std = @import("std");
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const options = b.addOptions();
    const llvm_module = build_llvm_module(b, target, optimize);

    const mod = b.createModule(.{
        .root_source_file = b.path("zag/zag.zig"),
        .target = target,
    });
    mod.addOptions("options", options);
    const exe = b.addExecutable(.{
        .name = "zag",
        .root_module = b.createModule(.{
            .root_source_file = b.path("zag/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = mod },
            },
        }),
    });
    exe.root_module.addOptions("options", options);
    b.installArtifact(exe);
    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const mod_tests = b.addTest(.{
        .root_module = mod,
    });
    mod_tests.root_module.addOptions("options", options);
    const run_mod_tests = b.addRunArtifact(mod_tests);
    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });
    const run_exe_tests = b.addRunArtifact(exe_tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_mod_tests.step);
    test_step.dependOn(&run_exe_tests.step);

    var includeLLVM = false;
    if (b.option(bool, "llvm", "Include LLVM in build") orelse false) {
        includeLLVM = true;
    }
    options.addOption(bool, "includeLLVM", includeLLVM);
    const git_version = b.run(&.{ "git", "log", "--pretty=format:%cI-%h", "-1" });
    options.addOption([]const u8, "git_version", git_version);

    if (includeLLVM) {
        mod.addImport("llvm-build-module", llvm_module);
        exe.root_module.addImport("llvm-build-module", llvm_module);
    }

    const fib = b.addExecutable(.{
        .name = "fib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/fib.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = mod },
            },
        }),
    });
    //b.step("fib", "Compile fib").dependOn(&b.installArtifact(fib).step);
    b.installArtifact(fib);
}

fn build_llvm_module(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) *std.Build.Module {
    const llvm_module = b.addModule("llvm-build-module", .{
        .root_source_file = b.path("./zag/libs/zig-llvm/src/llvm.zig"),
        .target = target,
        .optimize = optimize,
    });
    llvm_module.addCMacro("_FILE_OFFSET_BITS", "64");
    llvm_module.addCMacro("__STDC_CONSTANT_MACROS", "");
    llvm_module.addCMacro("__STDC_FORMAT_MACROS", "");
    llvm_module.addCMacro("__STDC_LIMIT_MACROS", "");
    llvm_module.linkSystemLibrary("z", .{});

    if (target.result.abi != .msvc) {
        llvm_module.link_libc = true;
    } else {
        llvm_module.link_libcpp = true;
    }

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
    return llvm_module;
}
