/**
 * AffineScript High-Assurance DOM Connector
 * (c) 2026 hyperpolymath
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

extern type Element;
extern type Text;
extern type Event;

extern fn querySelector(selector: String) -> Option[Element];
extern fn createElement(tag: String) -> Element;
extern fn createTextNode(text: String) -> Text;
extern fn appendChild(parent: Element, child: Element) -> Void;
extern fn appendText(parent: Element, child: Text) -> Void;
extern fn setAttribute(el: Element, name: String, value: String) -> Void;
extern fn addEventListener(el: Element, event: String, callback: fn(Event) -> Void) -> Void;

// Safe mounter
pub fn mount(selector: String, root_element: Element) -> Result[Void, String] {
    match querySelector(selector) {
        Some(parent) -> {
            appendChild(parent, root_element);
            Ok(())
        }
        None -> Err("Target selector not found: " + selector)
    }
}

// Fluent API for creating elements
pub fn h(tag: String, attrs: { id: String }, children: List[Element]) -> Element {
    let el = createElement(tag);
    if attrs.id != "" {
        setAttribute(el, "id", attrs.id);
    }
    // TODO: Iterate children and append
    el
}
