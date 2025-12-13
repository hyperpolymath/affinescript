# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

If you discover a security vulnerability in AffineScript, please report it by:

1. **Do NOT** open a public GitHub issue
2. Email the maintainers directly (see GitHub profile)
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Suggested fix (if any)

We will respond within 48 hours and work with you to understand and address the issue.

## Security Considerations

AffineScript is a compiler that:
- Reads source files from disk
- Produces WebAssembly output
- Does not execute network operations
- Does not execute arbitrary code during compilation

The primary security concerns are:
- Malicious input causing compiler crashes (DoS)
- Generated WASM with unintended behavior

We take these seriously and appreciate responsible disclosure.
