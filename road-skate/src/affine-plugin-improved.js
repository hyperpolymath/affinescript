/**
 * AffineScript Vite Plugin - Improved Version
 * (c) 2026 hyperpolymath
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

import { exec } from 'child_process';
import { promisify } from 'util';
import path from 'path';
import fs from 'fs';

const execAsync = promisify(exec);

async function compileAffineScript(filePath, outputDir, options) {
  const { compilerPath, wasmOptLevel, debug } = options;
  
  const outputFile = path.join(outputDir, path.basename(filePath, path.extname(filePath)) + '.wasm');
  const debugFlag = debug ? '--debug' : '';
  
  const command = `${compilerPath} --compile ${filePath} -o ${outputFile} --opt-level ${wasmOptLevel} ${debugFlag}`;
  
  try {
    console.log(`[affine-script] Executing: ${command}`);
    const { stdout, stderr } = await execAsync(command);
    
    if (stderr && !stderr.includes('warning')) {
      console.error(`[affine-script] Compiler stderr: ${stderr}`);
    }
    
    if (!fs.existsSync(outputFile)) {
      throw new Error(`Compilation failed: output file ${outputFile} not created`);
    }
    
    return { success: true, stdout, wasmPath: outputFile };
  } catch (error) {
    console.error(`[affine-script] Compilation failed: ${error.message}`);
    throw error;
  }
}

export default function affinePlugin(options = {}) {
  const {
    compilerPath = 'affinescript',
    wasmOptLevel = 3,
    debug = false,
    sourceMaps = true,
    watch = true
  } = options;

  return {
    name: 'affine-script',
    version: '1.0.0',
    
    config() {
      return {
        optimizeDeps: {
          include: ['affinescript-runtime'],
        },
        esbuild: {
          supported: {
            'affine-import': true,
          },
        },
      };
    },
    
    async transform(code, id) {
      if (id.endsWith('.as') || id.endsWith('.affine')) {
        console.log(`[affine-script] Compiling ${id}...`);
        
        const outputDir = path.dirname(id);
        const result = await compileAffineScript(id, outputDir, { compilerPath, wasmOptLevel, debug });
        
        const moduleName = path.basename(id, path.extname(id)).replace(/[^a-zA-Z0-9]/g, '_');
        
        // Generate JavaScript wrapper for WASM module
        const jsWrapper = `
import init, * as affineExports from '${result.wasmPath}';

await init();

export const ${moduleName} = affineExports;
export default ${moduleName};
`;
        
        return {
          code: jsWrapper,
          map: sourceMaps ? generateSourceMap(code, jsWrapper) : null
        };
      }
    },
    
    handleHotUpdate({ file, server, modules }) {
      if (file.endsWith('.as') || file.endsWith('.affine')) {
        console.log(`[affine-script] HMR: ${file} changed.`);
        
        // Find affected modules
        const affectedModules = modules.filter(m => 
          m.id.endsWith('.as') || m.id.endsWith('.affine') ||
          m.id.includes('affine')
        );
        
        if (affectedModules.length === 0) {
          // Fallback to full reload if no specific modules found
          server.ws.send({
            type: 'full-reload',
            path: '*'
          });
        } else {
          // Intelligent HMR updates
          return affectedModules.map(module => ({
            type: 'js-update',
            path: module.url,
            acceptedPath: module.url,
            timestamp: Date.now()
          }));
        }
      }
    },
    
    buildStart() {
      console.log('[affine-script] Build started - initializing AffineScript compiler');
      // Could add version checks, compiler initialization, etc.
    },
    
    buildEnd() {
      console.log('[affine-script] Build completed - optimizing WASM modules');
      // Could add WASM optimization, size reporting, etc.
    },
    
    configureServer(server) {
      if (watch) {
        // Set up file watching for .as files
        server.watcher.on('add', file => {
          if (file.endsWith('.as') || file.endsWith('.affine')) {
            console.log(`[affine-script] New file detected: ${file}`);
          }
        });
      }
    }
  };
}

// Simple source map generator (could be enhanced)
function generateSourceMap(originalCode, generatedCode) {
  return {
    version: 3,
    sources: ['original.as'],
    names: [],
    mappings: 'AAAA',
    file: 'compiled.js',
    sourcesContent: [originalCode]
  };
}