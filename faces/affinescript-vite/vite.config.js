import { defineConfig } from 'vite';
import affinePlugin from './src/index.js';

export default defineConfig({
  plugins: [
    affinePlugin()
  ],
  build: {
    lib: {
      entry: './src/index.js',
      name: 'AffineScriptVite',
      fileName: (format) => `affinescript-vite.${format}.js`
    },
    rollupOptions: {
      external: ['vite']
    }
  }
});
