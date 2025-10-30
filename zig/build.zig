const std = @import("std");
const Encoding = @import("zag/object/encoding.zig").Encoding;
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
    var includeLLVM = false;
    if (b.option(bool, "llvm", "Include LLVM in build") orelse false) {
        includeLLVM = true;
    }
    options.addOption(bool, "includeLLVM", includeLLVM);
    const git_version = b.run(&.{ "git", "log", "--pretty=format:%cI-%h", "-1" });
    options.addOption([]const u8, "git_version", git_version);
    const compile_date_with_extra = b.run(&.{ "date", "+%Y-%m-%dT%H:%M:%S%z" });
    const compile_date = std.mem.trim(u8, compile_date_with_extra, " \n\r");
    options.addOption([]const u8, "compile_date", compile_date);
    const encoding_option = b.option(Encoding, "encoding", "Object encoding");
    options.addOption(Encoding, "objectEncoding", encoding_option orelse Encoding.default());
    const max_classes = b.option(u16, "maxClasses", "Maximum number of classes") orelse 255;
    options.addOption(u16, "maxClasses", max_classes);
    const trace = b.option(bool, "trace", "trace execution") orelse false;
    options.addOption(bool, "trace", trace);
    const sequential = b.option(bool, "sequential", "sequential test") orelse false;

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
    const test_encodings: []const Encoding =
        if (encoding_option) |specific_encoding|
            &[_]Encoding{specific_encoding}
        else
            &[_]Encoding{
                .zag,
                //.nan, .zagAlt,
                //.ptr,
                //.spur
            };
    const test_step = b.step("test", "Run tests for all encoding types");

    for (test_encodings) |enc| {
        const enc_options = b.addOptions();
        enc_options.addOption(bool, "includeLLVM", includeLLVM);
        enc_options.addOption([]const u8, "git_version", git_version);
        enc_options.addOption([]const u8, "compile_date", compile_date);
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

        const test_module = b.createModule(.{
            .root_source_file = b.path("zag/test.zig"),
            .target = target,
            .optimize = optimize,
        });
        const enc_tests = if (trace or sequential)
            b.addTest(.{
                .root_module = test_module,
                .test_runner = .{ .path = b.path("test_runner.zig"), .mode = .simple },
            })
        else
            b.addTest(.{
                .root_module = test_module,
            });
        enc_tests.root_module.addOptions("options", enc_options);
        const run_enc_tests = b.addRunArtifact(enc_tests);
        test_step.dependOn(&run_enc_tests.step);
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
