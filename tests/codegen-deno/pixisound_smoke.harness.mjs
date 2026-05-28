// SPDX-License-Identifier: MPL-2.0
// bindings #2 — Node ESM harness for the @pixi/sound binding.
//
// Installs a globalThis.__as_pixi_sound mock before importing the
// generated module, drives each smoke* wrapper, and asserts the side
// effects (URL forwarded, transport calls counted, volume/loop set)
// were observed on the underlying mock sound instance.

import assert from "node:assert/strict";

class MockSound {
  constructor(url) {
    this.url = url;
    this.playCount = 0;
    this.stopCount = 0;
    this.pauseCount = 0;
    this.resumeCount = 0;
    this.volume = 1.0;
    this.loop = false;
  }
  play()   { this.playCount   += 1; }
  stop()   { this.stopCount   += 1; }
  pause()  { this.pauseCount  += 1; }
  resume() { this.resumeCount += 1; }
}

let lastFromUrl = null;

globalThis.__as_pixi_sound = {
  from(url) {
    lastFromUrl = url;
    return new MockSound(url);
  },
};

const {
  smokeFrom, smokePlay, smokeStop, smokePause, smokeResume,
  smokeSetVolume, smokeSetLoop,
} = await import("./pixisound_smoke.deno.js");

const s = smokeFrom("assets/bgm.mp3");
assert.equal(lastFromUrl, "assets/bgm.mp3", "from URL reaches host");
assert.equal(s.url, "assets/bgm.mp3", "constructed sound carries url");

assert.equal(smokePlay(s),   0, "play returns 0");
assert.equal(smokePlay(s),   0, "play returns 0 (twice)");
assert.equal(s.playCount,    2, "play invoked twice");

assert.equal(smokeStop(s),   0, "stop returns 0");
assert.equal(s.stopCount,    1, "stop invoked once");

assert.equal(smokePause(s),  0, "pause returns 0");
assert.equal(s.pauseCount,   1, "pause invoked once");

assert.equal(smokeResume(s), 0, "resume returns 0");
assert.equal(s.resumeCount,  1, "resume invoked once");

assert.equal(smokeSetVolume(s, 0.5), 0, "setVolume returns 0");
assert.equal(s.volume, 0.5, "volume field updated");

assert.equal(smokeSetLoop(s, true), 0, "setLoop returns 0");
assert.equal(s.loop, true, "loop field updated");

assert.equal(smokeSetLoop(s, false), 0, "setLoop(false) returns 0");
assert.equal(s.loop, false, "loop field cleared");

console.log("pixisound_smoke.harness.mjs OK");
