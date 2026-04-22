import roadSkate from './main.as';

// The roadSkate import is a WasmGC module wrapped by the affinescript-vite plugin
document.querySelector('#app').innerHTML = `
  <div id="game-container">
    <div id="road-skate-ui"></div>
  </div>
`;

// Initialize the AffineScript TEA program
roadSkate.init({
  node: document.querySelector('#road-skate-ui')
});
