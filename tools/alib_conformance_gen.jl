#!/usr/bin/env julia
# SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
#
# Generate AffineScript conformance tests from aLib specs

using YAML
using Markdown

"""
Generate AffineScript test from aLib YAML test case
"""
function generate_test_case(test_case::Dict, fn_name::String)
    input = test_case["input"]
    output = test_case["output"]
    desc = get(test_case, "description", "")

    # Convert YAML representation to AffineScript syntax
    collection = format_affinescript_value(input["collection"])
    fn_expr = parse_function_expression(input["fn"])
    expected = format_affinescript_value(output)

    """
    // $(desc)
    let input = $(collection);
    let result = $(fn_name)(input, $(fn_expr));
    assert_eq(result, $(expected), "$(desc)");
    """
end

"""
Format value as AffineScript literal
"""
function format_affinescript_value(val)
    if isa(val, Vector)
        if isempty(val)
            return "[]"
        end
        elements = join([format_affinescript_value(v) for v in val], ", ")
        return "[$(elements)]"
    elseif isa(val, String)
        return "\"$(val)\""
    elseif isa(val, Number)
        return string(val)
    else
        return string(val)
    end
end

"""
Parse function expression from aLib format to AffineScript
"""
function parse_function_expression(fn_str::String)
    # Convert "x => multiply(x, 2)" to "fn(x) => x * 2"
    # Convert "x => add(x, 10)" to "fn(x) => x + 10"
    # This is simplified - real implementation would parse properly

    fn_str = replace(fn_str, "multiply(x, " => "x * ")
    fn_str = replace(fn_str, "add(x, " => "x + ")
    fn_str = replace(fn_str, "concat(s, s)" => "s ++ s")
    fn_str = replace(fn_str, r"\)$" => "")

    # Convert arrow function to AffineScript syntax
    if occursin("=>", fn_str)
        parts = split(fn_str, "=>")
        param = strip(parts[1])
        body = strip(parts[2])
        return "fn($(param)) => $(body)"
    end

    return fn_str
end

"""
Generate complete conformance test file for a spec
"""
function generate_conformance_test(spec_path::String, output_dir::String)
    # Read the markdown spec file
    content = read(spec_path, String)

    # Extract YAML test cases (simplified - would use proper markdown parsing)
    yaml_match = match(r"```yaml\n(.*?)\n```"s, content)
    if yaml_match === nothing
        @warn "No YAML test cases found in $(spec_path)"
        return
    end

    yaml_str = yaml_match.captures[1]
    test_data = YAML.load(yaml_str)

    # Extract operation name from filename
    op_name = replace(basename(spec_path), ".md" => "")
    category = basename(dirname(spec_path))

    # Generate test file
    test_code = """
// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: $(category)/$(op_name)
// Source: aggregate-library/specs/$(category)/$(op_name).md

fn test_alib_$(category)_$(op_name)() -> TestResult {
"""

    # Generate test cases
    for test_case in test_data["test_cases"]
        test_code *= generate_test_case(test_case, op_name)
        test_code *= "\n"
    end

    test_code *= """
  Pass
}
"""

    # Write output file
    output_file = joinpath(output_dir, "$(category)_$(op_name).as")
    mkpath(output_dir)
    write(output_file, test_code)

    println("✓ Generated $(output_file)")
end

"""
Main function - generate conformance tests from aLib specs
"""
function main()
    if length(ARGS) < 2
        println("Usage: julia alib_conformance_gen.jl <alib-specs-dir> <output-dir>")
        println("Example: julia alib_conformance_gen.jl ../aggregate-library/specs tests/conformance")
        exit(1)
    end

    specs_dir = ARGS[1]
    output_dir = ARGS[2]

    if !isdir(specs_dir)
        @error "Specs directory not found: $(specs_dir)"
        exit(1)
    end

    println("Generating conformance tests from aLib specs...")
    println("Source: $(specs_dir)")
    println("Output: $(output_dir)")
    println()

    # Find all spec files
    spec_files = []
    for (root, dirs, files) in walkdir(specs_dir)
        for file in files
            if endswith(file, ".md")
                push!(spec_files, joinpath(root, file))
            end
        end
    end

    println("Found $(length(spec_files)) spec files\n")

    # Generate tests for each spec
    for spec_file in spec_files
        try
            generate_conformance_test(spec_file, output_dir)
        catch e
            @warn "Failed to generate test for $(spec_file): $(e)"
        end
    end

    println("\n✓ Generated $(length(spec_files)) conformance test files")
    println("Run tests with: affinescript test $(output_dir)")
end

# Run if executed directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
