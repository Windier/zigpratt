const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const tex_mod = b.addModule("tex", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const tex_lib = b.addLibrary(.{
        .name = "tex",
        .root_module = tex_mod,
        .linkage = .static,
    });
    b.installArtifact(tex_lib);

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addImport("tex", tex_mod);

    // Create the executable
    const exe = b.addExecutable(.{
        .name = "parser",
        .root_module = exe_mod,
    });

    b.installArtifact(exe);

    // Create WASM library
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });

    const wasm_lib = b.addExecutable(.{
        .name = "parser",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm.zig"),
            .target = wasm_target,
            .optimize = optimize,
        }),
    });
    wasm_lib.entry = .disabled;
    wasm_lib.rdynamic = true;

    const wasm_install = b.addInstallArtifact(wasm_lib, .{});
    const wasm_step = b.step("wasm", "Build WASM library");
    wasm_step.dependOn(&wasm_install.step);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Create the test executable
    const exe_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/parser.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
    try setupSnapshotTesting(b, target, exe);
}

fn setupSnapshotTesting(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    zemml_exe: *std.Build.Step.Compile,
) !void {
    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_allocator.deinit();

    const test_step = b.step("test-snapshots", "build snapshot tests and diff the results");

    const camera = b.addExecutable(.{
        .name = "camera",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/camera.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    const diff = b.addSystemCommand(&.{
        "git",
        "diff",
        "--cached",
        "--exit-code",
    });
    diff.addDirectoryArg(b.path("tests"));
    diff.setName("git diff tests/");
    test_step.dependOn(&diff.step);

    // We need to stage all of tests/ in order for untracked files to show up in
    // the diff. It's also not a bad automatism since it avoids the problem of
    // forgetting to stage new snapshot files.
    const git_add = b.addSystemCommand(&.{ "git", "add" });
    git_add.addDirectoryArg(b.path("tests/"));
    git_add.setName("git add tests/");
    diff.step.dependOn(&git_add.step);

    try setupSnapshotTestFolder(
        &arena_allocator,
        b,
        camera,
        zemml_exe,
        git_add,
        "tests/parse_ast",
        "--format=ast",
    );
}

fn setupSnapshotTestFolder(
    arena: *std.heap.ArenaAllocator,
    b: *std.Build,
    camera: *std.Build.Step.Compile,
    zemml_exe: *std.Build.Step.Compile,
    git_add: *std.Build.Step.Run,
    test_path: []const u8,
    format_arg: []const u8,
) !void {
    const tests_dir = try b.build_root.handle.openDir(test_path, .{
        .iterate = true,
    });

    var it = tests_dir.iterateAssumeFirstIteration();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        const src_path = b.pathJoin(&.{ test_path, entry.name });

        _ = arena.reset(.retain_capacity);

        const snap_name = try std.fmt.allocPrint(arena.allocator(), "{s}.snapshot.txt", .{entry.name});
        const snap_path = b.pathJoin(&.{ test_path, "snapshots", snap_name });
        const input_arg = try std.fmt.allocPrint(arena.allocator(), "--input={s}", .{src_path});
        // const output_arg = try std.fmt.allocPrint(arena.allocator(), "--output={s}", .{snap_path});

        const run_camera = b.addRunArtifact(camera);
        run_camera.addArtifactArg(zemml_exe);
        run_camera.addArg(input_arg);
        run_camera.addArg(format_arg);
        // run_camera.addArg(output_arg);
        run_camera.has_side_effects = true;

        const stdout = run_camera.captureStdErr();
        const update_snap = b.addUpdateSourceFiles();
        update_snap.addCopyFileToSource(stdout, snap_path);

        update_snap.step.dependOn(&run_camera.step);
        git_add.step.dependOn(&update_snap.step);
    }
}
