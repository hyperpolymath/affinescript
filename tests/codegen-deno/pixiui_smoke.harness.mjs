// SPDX-License-Identifier: MPL-2.0
// bindings #3 — Node ESM harness for the @pixi/ui binding.
//
// Installs a globalThis.__as_pixi_ui mock covering Button /
// FancyButton / Slider / Switch, drives the smoke functions, and
// asserts every constructor + callback registration + upcast was
// observed. Upcasts are identity at the JS level (the @pixi/ui
// classes are PIXI.Container subclasses); the harness mirrors that.

import assert from "node:assert/strict";

const ctorCalls = { Button: [], FancyButton: [], Slider: [], Switch: [] };
const onPressRegs = [];
const onUpdateRegs = [];
const onChangeRegs = [];

// `Signal`-ish stub matching the @pixi/ui event surface
// (`button.onPress.connect(cb)`). Records every registered callback
// into the sink array passed in at construction time.
class MockSignal {
  constructor(sink) { this._sink = sink; }
  connect(cb) { this._sink.push(cb); }
}

class MockButton {
  constructor(options) {
    ctorCalls.Button.push(options);
    this.onPress = new MockSignal(onPressRegs);
  }
}

class MockFancyButton {
  constructor(options) {
    ctorCalls.FancyButton.push(options);
  }
}

class MockSlider {
  constructor(options) {
    ctorCalls.Slider.push(options);
    this.onUpdate = new MockSignal(onUpdateRegs);
  }
}

class MockSwitch {
  constructor(options) {
    ctorCalls.Switch.push(options);
    this.onChange = new MockSignal(onChangeRegs);
  }
}

globalThis.__as_pixi_ui = {
  Button: MockButton,
  FancyButton: MockFancyButton,
  Slider: MockSlider,
  Switch: MockSwitch,
};

const { smokeButton, smokeFancyButton, smokeSlider, smokeSwitch } =
  await import("./pixiui_smoke.deno.js");

// ── Button: ctor + onPress + upcast ────────────────────────────────
const buttonPressCb = () => "pressed";
const buttonContainer = smokeButton({ text: "OK" }, buttonPressCb);
assert.equal(ctorCalls.Button.length, 1, "Button ctor called once");
assert.deepEqual(ctorCalls.Button[0], { text: "OK" }, "Button options reach host");
assert.equal(onPressRegs.length, 1, "onPress.connect called once");
assert.equal(onPressRegs[0], buttonPressCb, "onPress callback identity preserved");
assert.ok(buttonContainer instanceof MockButton, "Button upcast is identity (still MockButton)");

// ── FancyButton: ctor + upcast ─────────────────────────────────────
const fancyContainer = smokeFancyButton({ text: "Start", padding: 8 });
assert.equal(ctorCalls.FancyButton.length, 1, "FancyButton ctor called once");
assert.deepEqual(ctorCalls.FancyButton[0], { text: "Start", padding: 8 }, "FancyButton options reach host");
assert.ok(fancyContainer instanceof MockFancyButton, "FancyButton upcast is identity");

// ── Slider: ctor + onUpdate + upcast ───────────────────────────────
const sliderUpdateCb = (v) => v * 2;
const sliderContainer = smokeSlider({ min: 0, max: 100, value: 50 }, sliderUpdateCb);
assert.equal(ctorCalls.Slider.length, 1, "Slider ctor called once");
assert.deepEqual(ctorCalls.Slider[0], { min: 0, max: 100, value: 50 }, "Slider options reach host");
assert.equal(onUpdateRegs.length, 1, "onUpdate.connect called once");
assert.equal(onUpdateRegs[0], sliderUpdateCb, "onUpdate callback identity preserved");
assert.ok(sliderContainer instanceof MockSlider, "Slider upcast is identity");

// ── Switch: ctor + onChange + upcast ───────────────────────────────
const switchChangeCb = (state) => !state;
const switchContainer = smokeSwitch({ value: false }, switchChangeCb);
assert.equal(ctorCalls.Switch.length, 1, "Switch ctor called once");
assert.deepEqual(ctorCalls.Switch[0], { value: false }, "Switch options reach host");
assert.equal(onChangeRegs.length, 1, "onChange.connect called once");
assert.equal(onChangeRegs[0], switchChangeCb, "onChange callback identity preserved");
assert.ok(switchContainer instanceof MockSwitch, "Switch upcast is identity");

console.log("pixiui_smoke.harness.mjs OK");
