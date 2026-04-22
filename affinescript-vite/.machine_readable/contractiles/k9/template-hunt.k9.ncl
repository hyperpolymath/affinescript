K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Hunt-level template: Full execution with Just recipes
# Security Level: Hunt (full system access)
# ⚠️ SIGNATURE REQUIRED - Review carefully before use

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "PENDING: describe component type (e.g., 'deployment', 'setup-script')",
    security = {
      leash = 'Hunt,
      trust_level = "full-system-access",
      allow_network = true,
      allow_filesystem_write = true,
      allow_subprocess = true,
      signature_required = true,
    },
    metadata = {
      name = "PENDING: component-name",
      version = "1.0.0",
      description = "PENDING: Detailed description of what this component does",
      author = "hyperpolymath <j.d.a.jewell@open.ac.uk>",
    },
    warnings = [
      "This component has full system access",
      "Only run from trusted sources with verified signatures",
      "Review all Just recipes before execution",
      "Use dry-run mode first: ./must --dry-run run your-file.k9.ncl",
    ],
    side_effects = [
      "PENDING: List what files/directories this creates or modifies",
      "PENDING: List what commands this executes",
      "PENDING: List what network access this requires",
    ],
  },

  # Configuration with contracts (Yard-level validation)
  config = {
    # Add your configuration here with appropriate contracts
    target_dir
      | String
      | std.string.NonEmpty
      = "target/k9-output",

    dry_run | Bool = false,

    # Add more config as needed
  },

  # Just recipes for execution
  # These run when: ./must run your-file.k9.ncl
  recipes = {
    # Main entry point (runs by default)
    default = {
      recipe = "PENDING: main-task",
      description = "PENDING: What the default recipe does",
    },

    # Define your recipes here
    "main-task" = {
      dependencies = ["check-prerequisites"],
      commands = [
        'echo "PENDING: Add your commands here"',
        # Example: Create directory
        # "mkdir -p %{config.target_dir}",
        # Example: Run a command
        # "just build",
        # Example: Conditional execution
        # "@if [ \"%{config.dry_run}\" = \"true\" ]; then echo '[DRY-RUN] Would execute'; else actual-command; fi",
      ],
    },

    "check-prerequisites" = {
      description = "Verify required tools and permissions",
      commands = [
        # Example: Check for required tools
        # "command -v git || (echo 'ERROR: git not found' && exit 1)",
        # Example: Check permissions
        # "[ -w %{config.target_dir} ] || (echo 'ERROR: Cannot write to target directory' && exit 1)",
        "echo '✓ Prerequisites checked"',
      ],
    },

    # Add more recipes as needed
    "build" = {
      description = "Build the project",
      commands = [
        'echo "PENDING: Add build commands"',
      ],
    },

    "deploy" = {
      description = "Deploy the application",
      dependencies = ["build"],
      commands = [
        'echo "PENDING: Add deployment commands"',
      ],
    },

    "clean" = {
      description = "Clean up generated files",
      commands = [
        "echo '⚠️  This will delete files - waiting 3 seconds...'",
        'echo "PENDING: Add cleanup commands"',
        # "rm -rf %{config.target_dir}",
      ],
    },
  },

  # Validation (Yard-level checks before Hunt execution)
  validation = {
    check_target_dir = std.string.length config.target_dir > 0,
    # Add more validation as needed
  },
}

# Usage:
# 1. Fill in PENDING items above
# 2. Define configuration with contracts
# 3. Implement Just recipes with your commands
# 4. Test with dry-run: ./must --dry-run run your-file.k9.ncl
# 5. Review dry-run output carefully
# 6. Sign the component: ./must sign your-file.k9.ncl
# 7. Distribute with signature: your-file.k9.ncl.sig
# 8. Users verify and run: ./must verify && ./must run your-file.k9.ncl
#
# Security checklist:
# ✓ All PENDING items filled in
# ✓ side_effects documented accurately
# ✓ Commands reviewed for safety
# ✓ No hardcoded secrets or credentials
# ✓ Proper error handling in recipes
# ✓ Tested in dry-run mode
# ✓ Component signed with trusted key
