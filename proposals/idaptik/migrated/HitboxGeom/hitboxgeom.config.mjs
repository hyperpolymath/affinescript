// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for HitboxGeom.affine.
// Oracles re-derive from HitboxGeomCoprocessor.res / Hitbox.res semantics.
//
// aabb_overlap(ax,ay,aw,ah,bx,by,bw,bh): 1 if AABB overlap, 0 otherwise.
//   Overlap: NOT (ax+aw<=bx || bx+bw<=ax || ay+ah<=by || by+bh<=ay).
// point_in_rect(px,py,rx,ry,rw,rh): 1 if px in [rx,rx+rw) and py in [ry,ry+rh).
// dist_sq(x1,y1,x2,y2): (x2-x1)^2 + (y2-y1)^2

export default {
  affine: "HitboxGeom.affine",
  cases: [
    {
      name: "aabb_overlap: axis-aligned sweep",
      export: "aabb_overlap",
      args: [
        { values: [0, 10, 20, 30] }, // ax
        { values: [0, 10, 20, 30] }, // ay
        { values: [10, 20] },        // aw
        { values: [10, 20] },        // ah
        { values: [0, 10, 20, 30] }, // bx
        { values: [0, 10, 20, 30] }, // by
        { values: [10, 20] },        // bw
        { values: [10, 20] },        // bh
      ],
      oracle: (ax, ay, aw, ah, bx, by, bw, bh) => {
        if (ax + aw <= bx || bx + bw <= ax) return 0;
        if (ay + ah <= by || by + bh <= ay) return 0;
        return 1;
      },
    },
    {
      // Hitbox.contains: INCLUSIVE bounds (px <= rx+rw, py <= ry+rh).
      // Re-decomposition uses > for exclusion, not >=, matching Hitbox.res exactly.
      name: "point_in_rect: point vs rect sweep (inclusive boundary)",
      export: "point_in_rect",
      args: [
        { values: [-5, 0, 5, 10, 15, 20, 25, 30] }, // px
        { values: [-5, 0, 5, 10, 15, 20, 25, 30] }, // py
        { values: [0, 10, 20] },                     // rx
        { values: [0, 10, 20] },                     // ry
        { values: [10, 20] },                        // rw
        { values: [10, 20] },                        // rh
      ],
      oracle: (px, py, rx, ry, rw, rh) => {
        // Hitbox.contains: px >= rx && px <= rx+rw && py >= ry && py <= ry+rh
        if (px < rx || px > rx + rw) return 0;
        if (py < ry || py > ry + rh) return 0;
        return 1;
      },
    },
    {
      name: "dist_sq: small coordinate grid",
      export: "dist_sq",
      args: [
        { values: [0, 10, 20] },
        { values: [0, 10, 20] },
        { values: [0, 10, 20, 30] },
        { values: [0, 10, 20, 30] },
      ],
      oracle: (x1, y1, x2, y2) => {
        const dx = x2 - x1;
        const dy = y2 - y1;
        return dx * dx + dy * dy;
      },
    },
  ],
};
