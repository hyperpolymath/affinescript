/**
 * AffineScript Vite Plugin
 * (c) 2026 hyperpolymath
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

export default function affinePlugin(options = {}) {
  const { compilerPath = 'affinescript' } = options;

  return {
    name: 'affine-script',
    version: '0.1.0',

    // Handle .as and .affine files
    transform(code, id) {
      if (id.endsWith('.as') || id.endsWith('.affine')) {
        // Log for transparency
        console.log(`[affine-script] Compiling ${id}...`);

        // Placeholder for actual AffineScript -> WASM/JS compilation
        // In a real implementation, this would call the compiler via WASM
        // or a shell command: `affinescript --compile ${id} -o ${id}.wasm`
        
        // For now, we wrap the source in a JavaScript comment and export it
        // This allows the dev server to at least load the file as a module.
        const compiledCode = `
/**
 * Compiled from AffineScript (Source-only scaffold)
 * File: ${id}
 */
const source = ${JSON.stringify(code)};
export default source;
`;
        return {
          code: compiledCode,
          map: null, // TODO: Implement source maps from compiler
        };
      }
    },

    // Hot Module Replacement (HMR) handle
    handleHotUpdate({ file, server }) {
      if (file.endsWith('.as') || file.endsWith('.affine')) {
        console.log(`[affine-script] HMR: ${file} changed.`);
        server.ws.send({
          type: 'full-reload',
          path: '*',
        });
      }
    },
  };
}
