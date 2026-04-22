/**
 * AffineScript PixiJS Connector
 * (c) 2026 hyperpolymath
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

// Externs for PixiJS types and functions
// These will be linked at runtime via Typed WASM imports

extern type Application;
extern type Sprite;
extern type Texture;
extern type Container;
extern type Graphics;

extern fn createApplication(options: { width: Int, height: Int, backgroundColor: Int }) -> Application;
extern fn getStage(app: Application) -> Container;
extern fn addChild(container: Container, child: Sprite) -> Void;
extern fn fromImage(url: String) -> Texture;
extern fn createSprite(texture: Texture) -> Sprite;

// High-level wrapper for cleaner usage
pub fn init_pixi(width: Int, height: Int) -> Application {
    createApplication({
        width: width,
        height: height,
        backgroundColor: 0x1099bb
    })
}

pub fn add_sprite(app: Application, url: String) -> Sprite {
    let texture = fromImage(url);
    let sprite = createSprite(texture);
    addChild(getStage(app), sprite);
    sprite
}
