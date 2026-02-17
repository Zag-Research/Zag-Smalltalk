const std = @import("std");
const Encoding = @import("zag/object/encoding.zig").Encoding;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build options
    const build_options = createBuildOptions(b);

    // LLVM module
    const llvm_module = buildLLVMModule(b, target, optimize);

    // Main zag module
    const zag = createZagModule(b, target, build_options, llvm_module);

    // Main executable
    createMainExecutable(b, target, optimize, zag, build_options, llvm_module);

    // Experiment executables
    createExperimentExecutables(b, target, optimize, build_options, zag);

    // Test and benchmark steps
    createTestStep(b, target, optimize, build_options, llvm_module);
    createBenchStep(b, target, optimize, build_options, zag);
}

fn createBuildOptions(b: *std.Build) BuildOptions {
    const include_llvm = b.option(bool, "llvm", "Include LLVM in build") orelse false;
    const git_version = b.run(&.{ "git", "log", "--pretty=format:%cI-%h", "-1" });
    const compile_date_with_extra = b.run(&.{ "date", "+%Y-%m-%dT%H:%M:%S%z" });
    const compile_date = std.mem.trim(u8, compile_date_with_extra, " \n\r");
    const encoding_option = b.option(Encoding, "encoding", "Object encoding");
    const max_classes = b.option(u16, "maxClasses", "Maximum number of classes") orelse 255;
    const trace = b.option(bool, "trace", "trace execution") orelse false;
    const quit_on_first_failure = b.option(bool, "quitOnFirstFailure", "Stop after first error");
    const omit_frame_pointer = false;

    return .{
        .include_llvm = include_llvm,
        .git_version = git_version,
        .compile_date = compile_date,
        .encoding_option = encoding_option,
        .max_classes = max_classes,
        .trace = trace,
        .quit_on_first_failure = quit_on_first_failure,
        .omit_frame_pointer = omit_frame_pointer,
    };
}

fn addCommonOptions(options: *std.Build.Step.Options, build_options: BuildOptions, encoding: Encoding) void {
    options.addOption(bool, "includeLLVM", build_options.include_llvm);
    options.addOption([]const u8, "git_version", build_options.git_version);
    options.addOption([]const u8, "compile_date", build_options.compile_date);
    options.addOption(Encoding, "objectEncoding", encoding);
    options.addOption(u16, "maxClasses", build_options.max_classes);
    options.addOption(bool, "trace", build_options.trace);
}

fn createZagModule(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    build_options: BuildOptions,
    llvm_module: *std.Build.Module,
) *std.Build.Module {
    const options = b.addOptions();
    const encoding = build_options.encoding_option orelse Encoding.default();
    addCommonOptions(options, build_options, encoding);

    const zag = b.createModule(.{
        .root_source_file = b.path("zag/zag.zig"),
        .target = target,
    });
    zag.addOptions("options", options);

    if (build_options.include_llvm) {
        zag.addImport("llvm-build-module", llvm_module);
    }

    return zag;
}

fn createMainExecutable(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    zag: *std.Build.Module,
    build_options: BuildOptions,
    llvm_module: *std.Build.Module,
) void {
    const options = b.addOptions();
    const encoding = build_options.encoding_option orelse Encoding.default();
    addCommonOptions(options, build_options, encoding);

    const exe = b.addExecutable(.{
        .name = "zag",
        .root_module = b.createModule(.{
            .root_source_file = b.path("zag/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        }),
        .use_llvm = true,
    });
    exe.root_module.addOptions("options", options);

    if (build_options.include_llvm) {
        exe.root_module.addImport("llvm-build-module", llvm_module);
    }

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
}

fn createExperimentExecutables(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    build_options: BuildOptions,
    zag: *std.Build.Module,
) void {
    const fib = b.addExecutable(.{
        .name = "fib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/fib.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        }),
        .use_llvm = true,
    });
    b.installArtifact(fib);

    const cnpFib = b.addExecutable(.{
        .name = "cnpFib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/cnpFib.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        }),
        .use_llvm = true,
    });
    b.installArtifact(cnpFib);

    const branchPrediction = b.addExecutable(.{
        .name = "branchPrediction",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/branchPrediction.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        }),
        .use_llvm = true,
    });
    _ = branchPrediction;

    const cnp = b.addExecutable(.{
        .name = "cnp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/cnp/cnp.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        }),
        .use_llvm = true,
    });
    b.installArtifact(cnp);
    const run_cnp = b.addRunArtifact(cnp);
    const run_cnp_step = b.step("cnp-run", "Build and run cnp (copy-and-patch JIT)");
    run_cnp_step.dependOn(&run_cnp.step);

    const cnp_bench = b.addExecutable(.{
        .name = "cnp-bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/cnp/bench.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        }),
        .use_llvm = true,
    });
    b.installArtifact(cnp_bench);
    const run_cnp_bench = b.addRunArtifact(cnp_bench);
    const run_cnp_bench_step = b.step("cnp-bench", "Run CNP JIT benchmarks");
    run_cnp_bench_step.dependOn(&run_cnp_bench.step);

    const cnp_fib_bench = b.addExecutable(.{
        .name = "cnp-fib-bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/cnp/fib_bench.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        }),
        .use_llvm = true,
    });
    b.installArtifact(cnp_fib_bench);
    const run_cnp_fib_bench = b.addRunArtifact(cnp_fib_bench);
    const run_cnp_fib_bench_step = b.step("cnp-fib-bench", "Run CNP JIT fibonacci benchmarks");
    run_cnp_fib_bench_step.dependOn(&run_cnp_fib_bench.step);

    const fib_check = b.addExecutable(.{
        .name = "fib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("experiments/fib.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        }),
        .use_llvm = true,
    });
    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&fib_check.step);
}

fn createTestStep(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    build_options: BuildOptions,
    llvm_module: *std.Build.Module,
) void {
    const test_encodings: []const Encoding =
        if (build_options.encoding_option) |specific_encoding|
            &[_]Encoding{specific_encoding}
        else
            &[_]Encoding{.zag};

    const test_step = b.step("test", "Run tests for all encoding types");
    const sequential = b.option(bool, "sequential", "sequential test") orelse false;

    for (test_encodings) |enc| {
        const enc_options = b.addOptions();
        addCommonOptions(enc_options, build_options, enc);

        if (build_options.quit_on_first_failure) |quit| {
            enc_options.addOption(bool, "quitOnFirstFailure", quit);
        }

        const zag_enc = b.createModule(.{
            .root_source_file = b.path("zag/zag.zig"),
            .target = target,
        });
        zag_enc.addOptions("options", enc_options);

        if (build_options.include_llvm) {
            zag_enc.addImport("llvm-build-module", llvm_module);
        }

        const test_module = b.createModule(.{
            .root_source_file = b.path("zag/test.zig"),
            .target = target,
            .optimize = optimize,
        });

        const enc_tests = if (build_options.trace or sequential)
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

fn createBenchStep(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    build_options: BuildOptions,
    zag: *std.Build.Module,
) void {
    const bench_encodings: []const Encoding =
        if (build_options.encoding_option) |specific_encoding|
            &[_]Encoding{specific_encoding}
        else
            &[_]Encoding{
                .nan,
                .zag,
                .zagAlt,
                .spur,
                .taggedInt,
                .ptr,
                .onlyInt,
                .onlyFloat,
            };

    const bench_step = b.step("bench", "Run bench for all encoding types");

    for (bench_encodings) |enc| {
        const enc_options = b.addOptions();
        addCommonOptions(enc_options, build_options, enc);

        const bench_module = b.createModule(.{
            .root_source_file = b.path("experiments/fib.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zag", .module = zag },
            },
            .omit_frame_pointer = build_options.omit_frame_pointer,
        });

        const enc_benchs = b.addExecutable(.{
            .name = "bench",
            .root_module = bench_module,
            .use_llvm = true,
        });
        enc_benchs.root_module.addOptions("options", enc_options);

        const run_enc_benchs = b.addRunArtifact(enc_benchs);
        bench_step.dependOn(&run_enc_benchs.step);
    }
}

fn buildLLVMModule(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) *std.Build.Module {
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
        .linux => llvm_module.linkSystemLibrary("LLVM-18", .{}),
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

const BuildOptions = struct {
    include_llvm: bool,
    git_version: []const u8,
    compile_date: []const u8,
    encoding_option: ?Encoding,
    max_classes: u16,
    trace: bool,
    quit_on_first_failure: ?bool,
    omit_frame_pointer: bool,
};
