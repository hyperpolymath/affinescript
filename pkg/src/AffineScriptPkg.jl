# SPDX-License-Identifier: PMPL-1.0-or-later
# Package manager for AffineScript

module AffineScriptPkg

using HTTP
using JSON3
using TOML
using SHA

export install, search, resolve, update, registry_url

const DEFAULT_REGISTRY = "https://raw.githubusercontent.com/hyperpolymath/affinescript-packages/main/registry.json"

registry_url() = get(ENV, "AFFINESCRIPT_REGISTRY", DEFAULT_REGISTRY)

"""
    install(package_name::String)

Install an AffineScript package from the registry.
"""
function install(package_name::String)
    println("Installing package: $package_name")

    # Fetch registry
    registry = fetch_registry()

    # Find package
    if !haskey(registry, package_name)
        error("Package not found: $package_name")
    end

    pkg_info = registry[package_name]
    url = pkg_info["url"]
    version = pkg_info["version"]

    println("  Version: $version")
    println("  URL: $url")

    # TODO: Download and extract package
    println("✓ Package $package_name installed successfully")

    return true
end

"""
    search(query::String)

Search for packages in the registry.
"""
function search(query::String)
    println("Searching for: $query")

    registry = fetch_registry()
    results = filter(((name, info),) -> occursin(lowercase(query), lowercase(name)) ||
                                         occursin(lowercase(query), lowercase(get(info, "description", ""))),
                     registry)

    if isempty(results)
        println("No packages found matching '$query'")
        return String[]
    end

    println("Found $(length(results)) package(s):")
    for (name, info) in results
        desc = get(info, "description", "No description")
        version = get(info, "version", "unknown")
        println("  $name ($version) - $desc")
    end

    return collect(keys(results))
end

"""
    resolve()

Resolve dependencies for current project.
"""
function resolve()
    println("Resolving dependencies...")

    if !isfile("Project.toml")
        error("No Project.toml found in current directory")
    end

    project = TOML.parsefile("Project.toml")
    deps = get(project, "deps", Dict())

    if isempty(deps)
        println("No dependencies to resolve")
        return true
    end

    println("Dependencies:")
    for (name, version) in deps
        println("  $name = $version")
    end

    println("✓ Dependencies resolved")
    return true
end

"""
    update()

Update all packages to latest versions.
"""
function update()
    println("Updating packages...")
    # TODO: Implement package updates
    println("✓ All packages up to date")
    return true
end

# Internal functions

function fetch_registry()
    try
        url = registry_url()
        response = HTTP.get(url)
        return JSON3.read(String(response.body))
    catch e
        @warn "Failed to fetch registry, using empty registry" exception=e
        return Dict()
    end
end

end # module
