const std = @import("std");
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const options = b.addOptions();
    const llvm_module = build_llvm_module(b, target, optimize);

    const zag = b.createModule(.{
        .root_source_file = b.path("zag/zag.zig"),
        .target = target,
    });
    zag.addOptions("options", options);
    const exe = b.addExecutable(.{
        .name = "zag",
        .root_module = b.createModule(.{
            .root_source_file = b.path("zag/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
        }),
        .use_llvm = true,
    });
    exe.root_module.addOptions("options", options);
    //    b.installArtifact(exe);
    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("zag/root.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                //.{ .name = "zag", .module = zag },
            },
        }),
    });
    tests.root_module.addOptions("options", options);
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);

    var includeLLVM = false;
    if (b.option(bool, "llvm", "Include LLVM in build") orelse false) {
        includeLLVM = true;
    }
    options.addOption(bool, "includeLLVM", includeLLVM);
    const git_version = b.run(&.{ "git", "log", "--pretty=format:%cI-%h", "-1" });
    options.addOption([]const u8, "git_version", git_version);
    const compile_date = b.run(&.{ "date", "+%Y-%m-%dT%H:%M:%S%z" });
    options.addOption([]const u8, "compile_date", std.mem.trim(u8, compile_date, " \n\r"));
    const Encoding = @import("zag/encoding.zig").Encoding;
    const encoding = b.option(Encoding, "encoding", "Object encoding") orelse .zag;
    options.addOption(Encoding, "objectEncoding", encoding);
    const max_classes = b.option(u16, "maxClasses", "Maximum number of classes") orelse 255;
    options.addOption(u16, "maxClasses", max_classes);
    const trace = b.option(bool, "trace", "trace execution") orelse false;
    options.addOption(bool, "trace", trace);

    if (includeLLVM) {
        zag.addImport("llvm-build-module", llvm_module);
        exe.root_module.addImport("llvm-build-module", llvm_module);
    }

    const fib = b.addExecutable(.{
        .name = "fib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/fib.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
        }),
    });
    //b.step("fib", "Compile fib").dependOn(&b.installArtifact(fib).step);
    b.installArtifact(fib);

    const branchPrediction = b.addExecutable(.{
        .name = "branchPrediction",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/branchPrediction.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
        }),
    });
    _ = branchPrediction;
    //b.step("branchPrediction", "Compile branchPrediction").dependOn(&b.installArtifact(branchPrediction).step);
    //b.installArtifact(branchPrediction);

    const fib_check = b.addExecutable(.{
        .name = "fib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/fib.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
        }),
    });
    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&fib_check.step);

    // --- Encoding Tests ---
    const all_encodings = [_]Encoding{.spur}; // TODO: add more when ready
    const test_all_step = b.step("test-all-encodings", "Run tests for all encoding types");

    for (all_encodings) |enc| {
        const enc_name = @tagName(enc);

        const enc_options = b.addOptions();
        enc_options.addOption(bool, "includeLLVM", includeLLVM);
        enc_options.addOption([]const u8, "git_version", git_version);
        enc_options.addOption([]const u8, "compile_date", std.mem.trim(u8, compile_date, " \n\r"));
        enc_options.addOption(Encoding, "objectEncoding", enc);
        enc_options.addOption(u16, "maxClasses", max_classes);
        enc_options.addOption(bool, "trace", trace);

        const zag_enc = b.createModule(.{
            .root_source_file = b.path("zag/zag.zig"),
            .target = target,
        });

        zag_enc.addOptions("options", enc_options);

        if (includeLLVM) {
            zag_enc.addImport("llvm-build-module", llvm_module);
        }

        const enc_tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("zag/root.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    // .{ .name = "zag", .module = zag_enc },
                },
            }),
        });
        enc_tests.root_module.addOptions("options", enc_options);

        const run_enc_tests = b.addRunArtifact(enc_tests);
        const enc_step = b.step(b.fmt("test-{s}", .{enc_name}), b.fmt("Test {s} encoding", .{enc_name}));
        enc_step.dependOn(&run_enc_tests.step);
        test_all_step.dependOn(&run_enc_tests.step);
    }
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
