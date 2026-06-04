// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
import gameLogic from './logic.as';

document.querySelector('#app').textContent = `
  <h1>AffineScript + Vite</h1>
  <p>Status: Game logic loaded!</p>
  <pre>${gameLogic}</pre>
`;
