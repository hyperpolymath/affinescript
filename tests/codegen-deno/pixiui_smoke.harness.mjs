// SPDX-License-Identifier: MPL-2.0
// bindings #3 — Node ESM harness for the @pixi/ui binding.
//
// Installs a globalThis.__as_pixi_ui mock covering Button /
// FancyButton / Slider / Switch, drives the smoke functions, and
// asserts every constructor + callback registration + upcast was
// observed. Upcasts are identity at the JS level (the @pixi/ui
// classes are PIXI.Container subclasses); the harness mirrors that.

import assert from "node:assert/strict";

const ctorCalls = { Button: [], FancyButton: [], Slider: [], Switch: [], ProgressBar: [], List: [], Input: [] };
const onPressRegs = [];
const onUpdateRegs = [];
const onChangeRegs = [];
const fancyPressRegs = [];
const fancyDownRegs = [];
const fancyHoverRegs = [];
const inputEnterRegs = [];
const inputChangeRegs = [];
const listChildren = [];
const sliderChildren = [];

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
    this.onPress = new MockSignal(fancyPressRegs);
    this.onDown = new MockSignal(fancyDownRegs);
    this.onHover = new MockSignal(fancyHoverRegs);
  }
}

class MockSlider {
  constructor(options) {
    ctorCalls.Slider.push(options);
    this.onUpdate = new MockSignal(onUpdateRegs);
    this.value = options?.value ?? 0;
  }
  addChild(child) { sliderChildren.push(child); return child; }
}

class MockSwitch {
  constructor(options) {
    ctorCalls.Switch.push(options);
    this.onChange = new MockSignal(onChangeRegs);
  }
}

class MockProgressBar {
  constructor(options) {
    ctorCalls.ProgressBar.push(options);
    this.progress = options?.progress ?? 0;
  }
}
class MockList {
  constructor(options) { ctorCalls.List.push(options); }
  addChild(child) { listChildren.push(child); return child; }
}
class MockInput {
  constructor(options) {
    ctorCalls.Input.push(options);
    this.value = options?.value ?? "";
    this.onEnter = new MockSignal(inputEnterRegs);
    this.onChange = new MockSignal(inputChangeRegs);
  }
}

globalThis.__as_pixi_ui = {
  Button: MockButton,
  FancyButton: MockFancyButton,
  Slider: MockSlider,
  Switch: MockSwitch,
  ProgressBar: MockProgressBar,
  List: MockList,
  Input: MockInput,
};

const { smokeButton, smokeFancyButton, smokeSlider, smokeSwitch, smokeProgressBar, smokeList, smokeInput, smokeFancyButtonEvents, smokeSliderValue, smokeSliderAdd, smokeListAdd, smokeInputValue, smokeProgress } =
  await import("./pixiui_smoke.bun.js");

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

const pb = smokeProgressBar({ progress: 0.4 });
assert.equal(ctorCalls.ProgressBar.length, 1, "ProgressBar ctor called once");
assert.deepEqual(ctorCalls.ProgressBar[0], { progress: 0.4 }, "ProgressBar options reach host");
assert.ok(pb instanceof MockProgressBar, "ProgressBar upcast is identity");

const list = smokeList({ type: "vertical" });
assert.equal(ctorCalls.List.length, 1, "List ctor called once");
assert.ok(list instanceof MockList, "List upcast is identity");

const input = smokeInput({ placeholder: "name" });
assert.equal(ctorCalls.Input.length, 1, "Input ctor called once");
assert.ok(input instanceof MockInput, "Input upcast is identity");

console.log("pixiui_smoke.harness.mjs OK");
