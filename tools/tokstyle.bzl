load("@rules_cc//cc:action_names.bzl", "C_COMPILE_ACTION_NAME")
load("@rules_cc//cc:defs.bzl", "CcInfo", "cc_common")
load("@rules_cc//cc:toolchain_utils.bzl", "find_cpp_toolchain")

def _tokstyle_c_test_impl(ctx):
    cc_toolchain = find_cpp_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    c_compiler_path = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
    )

    cc_info = cc_common.merge_cc_infos(direct_cc_infos = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep])
    compilation_context = cc_info.compilation_context

    preprocessed_files = []
    all_srcs = depset(ctx.files.srcs)
    for src in ctx.files.srcs:
        if src.extension != "c":
            continue

        out = ctx.actions.declare_file(src.path[:-1] + "i")
        preprocessed_files.append(out)

        args = ctx.actions.args()
        args.add("-E")
        args.add("-undef")
        args.add("-DCMP_NO_FLOAT")
        args.add("-DWORDS_BIGENDIAN=0")
        args.add("-o", out)
        args.add("-Ihs-tokstyle/include")
        args.add("-I.")
        args.add("-iquote", ".")

        # Add current directory as both -I and -iquote for safety
        args.add("-I" + src.dirname)
        args.add("-iquote", src.dirname)

        for inc in ctx.attr.includes:
            if inc == ".":
                args.add("-I" + ctx.label.package)
            else:
                args.add("-I" + ctx.label.package + "/" + inc)

        for dc in ctx.attr.defines + ctx.attr.local_defines:
            args.add("-D" + dc)

        for inc in compilation_context.includes.to_list():
            args.add("-I" + inc)
        for inc in compilation_context.system_includes.to_list():
            args.add("-isystem" + inc)
        for inc in compilation_context.quote_includes.to_list():
            args.add("-iquote" + inc)
        for dc in compilation_context.defines.to_list():
            args.add("-D" + dc)

        args.add_all(ctx.attr.copts)
        args.add(src)

        ctx.actions.run(
            outputs = [out],
            inputs = depset(
                [src],
                transitive = [
                    compilation_context.headers,
                    cc_toolchain.all_files,
                    all_srcs,
                ],
            ),
            executable = c_compiler_path,
            arguments = [args],
            mnemonic = "CPreprocess",
            use_default_shell_env = True,
        )

    exe = ctx.actions.declare_file(ctx.label.name)

    # Build the final command line
    test_args = []
    test_args.append("--include=hs-tokstyle/include")
    test_args.extend([f.short_path for f in preprocessed_files])

    # We use a shell script wrapper because it's a test
    ctx.actions.write(
        output = exe,
        content = """#!/bin/bash
exec {tool} {args} "$@"
""".format(
            tool = ctx.executable._tool.short_path,
            args = " ".join(test_args),
        ),
        is_executable = True,
    )

    runfiles = ctx.runfiles(files = preprocessed_files + ctx.files.srcs + [ctx.executable._tool])

    return [DefaultInfo(
        executable = exe,
        runfiles = runfiles,
    )]

tokstyle_c_test = rule(
    implementation = _tokstyle_c_test_impl,
    test = True,
    attrs = {
        "srcs": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [CcInfo]),
        "copts": attr.string_list(),
        "defines": attr.string_list(),
        "local_defines": attr.string_list(),
        "includes": attr.string_list(),
        "_tool": attr.label(
            default = Label("//hs-tokstyle/tools:check-c"),
            executable = True,
            cfg = "exec",
        ),
        "_cc_toolchain": attr.label(default = Label("@rules_cc//cc:current_cc_toolchain")),
    },
    fragments = ["cpp"],
    toolchains = ["@rules_cc//cc:toolchain_type"],
)
