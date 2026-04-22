# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) {{CURRENT_YEAR}} hyperpolymath (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Containerfile for AffineScript-Vite
# Build: podman build -t affinescript-vite:latest -f Containerfile .
# Run:   podman run --rm -it affinescript-vite:latest
# Seal:  selur seal affinescript-vite:latest

# --- Build stage ---
FROM cgr.dev/chainguard/wolfi-base:latest AS build

# TODO: Install build dependencies for your stack
# Examples:
#   RUN apk add --no-cache rust cargo       # Rust
#   RUN apk add --no-cache elixir erlang    # Elixir
#   RUN apk add --no-cache zig              # Zig

WORKDIR /build
COPY . .

# TODO: Replace with your build command
# Examples:
#   RUN cargo build --release
#   RUN mix deps.get && MIX_ENV=prod mix release
#   RUN zig build -Doptimize=ReleaseSafe

# --- Runtime stage ---
FROM cgr.dev/chainguard/static:latest

# Copy built artifact from build stage
# TODO: Replace with your binary/artifact path
# Examples:
#   COPY --from=build /build/target/release/affinescript-vite /usr/local/bin/
#   COPY --from=build /build/_build/prod/rel/affinescript-vite /app/
#   COPY --from=build /build/zig-out/bin/affinescript-vite /usr/local/bin/

# Non-root user (chainguard images default to nonroot)
USER nonroot

# TODO: Replace with your entrypoint
# ENTRYPOINT ["/usr/local/bin/affinescript-vite"]
