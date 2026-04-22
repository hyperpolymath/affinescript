# AffineScript Game Controls Reference

## 🎮 Basic Movement Controls

### Standard Navigation
```
┌─────────────────────────────────────┐
│         AFFINESCRIPT CONTROLS        │
├─────────────────┬───────────────────┤
│  MOVEMENT       │  ACTIONS          │
├─────────────────┼───────────────────┤
│  ←  →           │  Turn Left/Right  │
│  ↑  ↓           │  Accelerate       │
│                 │  Forward/Backward │
├─────────────────┼───────────────────┤
│  P             │  Periscope Mode   │
│  E             │  Embark/Land      │
│  M             │  Disembark (Water)│
│  TAB           │  Eject (Air)      │
│  SPACE         │  Fire Torpedo     │
│  ENTER         │  Fire Missile    │
│  SHIFT         │  Depth Charge     │
├─────────────────┼───────────────────┤
│  J  L          │  Strafe Left/Right│
│  (on land)     │                   │
└─────────────────┴───────────────────┘
```

---

## 🌊 Submarine Controls

### Underwater Navigation
- **← → Arrow Keys**: Turn submarine left/right
- **↑ ↓ Arrow Keys**: Accelerate forward/backward
- **P**: Toggle periscope mode (invisible to surface detection)
  - Works only when sitting in water
  - Toggle on/off with same key
- **M**: Disembark as deep sea diver
  - Must be underwater
  - Equips diving gear automatically

### Combat
- **SPACE**: Fire torpedo
  - Limited ammunition
  - Straight-line trajectory
  - Effective against ships and subs
- **SHIFT**: Drop depth charge
  - Anti-submarine weapon
  - Sinks to maximum depth
  - Area effect damage

---

## 🚀 Spaceflight Controls

### Atmospheric Flight
- Maintain **at least 88 mph** (142 km/h)
- Make **sharp turn upward** at top of screen
- Transition to space mode automatically

### Space Navigation
- **← → ↑ ↓**: Standard movement
- **1-9**: Autopilot to planets
  - **1**: Mercury
  - **2**: Venus
  - **3**: Earth
  - **4**: Mars
  - **5**: Jupiter
  - **6**: Saturn
  - **7**: Uranus
  - **8**: Neptune
  - **9**: Pluto (if included)
- **ALT GR (Right Alt)**: Warp speed
  - Opens galaxy map
  - Enables interstellar travel
  - Consumes fuel rapidly

### Planetary Approach
- Navigate to planet vicinity
- Press **E** for interaction menu:
  - **Enter**: Land on planet
  - **Orbit**: Establish stable orbit
  - **Depart**: Leave planetary system

### Space Hazards
- **⚠️ Sun**: Instant death on contact
- **⚠️ Asteroids**: Collision damage
- **⚠️ Black Holes**: Irreversible pull
- **⚠️ Solar Flares**: Temporary system damage

---

## 🌍 Land Vehicle Controls

### Ground Movement
- **← → ↑ ↓**: Standard movement
- **E**: Embark/Dembark
  - Board nearby vehicles
  - Exit current vehicle
  - Enter buildings
- **J**: Strafe left
- **L**: Strafe right
- **SPACE**: Primary weapon (when implemented)
- **ENTER**: Secondary weapon (when implemented)

---

## 🛡️ Combat Controls (Planned)

### Current Implementation
```
┌─────────────────────────────────────┐
│         CURRENT COMBAT             │
├─────────────────┬───────────────────┤
│  WEAPON         │  CONTROL          │
├─────────────────┼───────────────────┤
│  Torpedo        │  SPACE            │
│  Missile        │  ENTER            │
│  Depth Charge   │  SHIFT            │
├─────────────────┼───────────────────┤
│  Targeting      │  Mouse            │
│  Fire           │  Left Click       │
│  Special        │  Right Click      │
└─────────────────┴───────────────────┘
```

### Planned Features
- **T**: Toggle targeting system
- **R**: Reload weapons
- **Q**: Cycle weapons
- **F**: Use special ability
- **C**: Change camera view
- **V**: Toggle HUD display

---

## 🎮 Vehicle-Specific Controls

### Submarine
```
┌─────────────────────────────────────┐
│          SUBMARINE CONTROLS        │
├─────────────────┬───────────────────┤
│  ACTION         │  CONTROL          │
├─────────────────┼───────────────────┤
│  Dive           │  ↓ (hold)         │
│  Surface        │  ↑ (hold)         │
│  Periscope      │  P                │
│  Silent Running │  S                │
│  Sonar          │  O                │
│  Torpedo        │  SPACE            │
│  Mine           │  N                │
│  Eject          │  TAB              │
└─────────────────┴───────────────────┘
```

### Aircraft
```
┌─────────────────────────────────────┐
│           AIRCRAFT CONTROLS        │
├─────────────────┬───────────────────┤
│  ACTION         │  CONTROL          │
├─────────────────┼───────────────────┤
│  Ascend         │  ↑                │
│  Descend        │  ↓                │
│  Roll Left      │  Q                │
│  Roll Right     │  E                │
│  Afterburner    │  SHIFT            │
│  Missile        │  ENTER            │
│  Eject          │  TAB              │
│  Landing Gear    │  G                │
└─────────────────┴───────────────────┘
```

### Spacecraft
```
┌─────────────────────────────────────┐
│          SPACECRAFT CONTROLS       │
├─────────────────┬───────────────────┤
│  ACTION         │  CONTROL          │
├─────────────────┼───────────────────┤
│  Thrust          │  ↑                │
│  Reverse         │  ↓                │
│  Yaw Left        │  ←                │
│  Yaw Right       │  →                │
│  Warp Drive      │  ALT GR           │
│  Shields         │  H                │
│  Target Lock     │  T                │
│  Dock            │  D                │
└─────────────────┴───────────────────┘
```

---

## 🗺️ Navigation Reference

### Movement Modes
```
┌─────────────────────────────────────┐
│          NAVIGATION MODES          │
├─────────────────┬───────────────────┤
│  MODE           │  CONTROLS          │
├─────────────────┼───────────────────┤
│  Submarine       │  Arrow Keys       │
│  Land Vehicle    │  Arrow Keys + J/L│
│  Aircraft        │  Arrow Keys + Q/E│
│  Spacecraft      │  Arrow Keys       │
│  On Foot         │  Arrow Keys       │
│  Diving          │  WASD (planned)   │
└─────────────────┴───────────────────┘
```

### Transition Commands
```
┌─────────────────────────────────────┐
│         TRANSITION COMMANDS        │
├─────────────────┬───────────────────┤
│  FROM → TO      │  CONTROL          │
├─────────────────┼───────────────────┤
│  Sub → Diver     │  M (underwater)   │
│  Diver → Sub     │  E (near sub)     │
│  Sub → Land      │  E (near shore)   │
│  Land → Sub      │  E (in water)     │
│  Any → Eject     │  TAB (in air)     │
│  Atmosphere →    │  88 mph + ↑       │
│   Space          │                   │
│  Space → Planet  │  1-9 + E          │
│  Planet → Orbit  │  E → Orbit        │
└─────────────────┴───────────────────┘
```

---

## 🎯 Targeting & Combat

### Current Weapon Systems
```
┌─────────────────────────────────────┐
│           WEAPON SYSTEMS            │
├─────────────────┬───────────────────┤
│  WEAPON         │  DETAILS          │
├─────────────────┼───────────────────┤
│  Torpedo        │  SPACE            │
│                 │  - Straight line   │
│                 │  - Underwater only │
│                 │  - Limited range   │
├─────────────────┼───────────────────┤
│  Missile        │  ENTER            │
│                 │  - Air/space only  │
│                 │  - Heat-seeking    │
│                 │  - Limited ammo    │
├─────────────────┼───────────────────┘
│  Depth Charge   │  SHIFT            │
│                 │  - Anti-submarine  │
│                 │  - Sinks to bottom │
│                 │  - Area effect     │
└─────────────────┴───────────────────┘
```

### Planned Combat Features
- **Laser**: Continuous beam weapon
- **Railgun**: High-velocity projectile
- **EM Pulse**: Disables electronics
- **Mine**: Deployable explosive
- **Decoy**: Distraction flare
- **ECM**: Electronic countermeasures

---

## 🌌 Space Travel Guide

### Solar System Navigation
```
┌─────────────────────────────────────┐
│        SPACE NAVIGATION            │
├─────────────────┬───────────────────┤
│  DESTINATION    │  CONTROL          │
├─────────────────┼───────────────────┤
│  Mercury         │  1                │
│  Venus           │  2                │
│  Earth           │  3                │
│  Mars            │  4                │
│  Jupiter         │  5                │
│  Saturn          │  6                │
│  Uranus          │  7                │
│  Neptune         │  8                │
│  Pluto           │  9                │
├─────────────────┼───────────────────┤
│  Warp Speed      │  ALT GR           │
│  Galaxy Map      │  ALT GR (hold)    │
│  Solar Orbit     │  O                │
│  Planet Orbit    │  P                │
│  Dock            │  D                │
└─────────────────┴───────────────────┘
```

### Interstellar Travel
1. **Reach orbit** of current planet
2. **Press ALT GR** to engage warp drive
3. **Hold ALT GR** to open galaxy map
4. **Select destination** star system
5. **Confirm travel** (fuel check)
6. **Warp transition** animation
7. **Arrive** in new system

---

## ⚠️ Hazards & Warnings

### Environmental Hazards
```
┌─────────────────────────────────────┐
│           ENVIRONMENTAL RISKS      │
├─────────────────┬───────────────────┤
│  HAZARD         │  EFFECT           │
├─────────────────┼───────────────────┤
│  Sun Contact    │  Instant death    │
│  Deep Space     │  Oxygen depletion │
│  Black Hole     │  Irreversible pull │
│  Asteroid Field │  Collision damage  │
│  Solar Flare    │  Systems failure   │
│  Radiation Belt │  Health damage    │
│  Extreme Depth  │  Pressure damage   │
│  Surface        │  Detection risk   │
└─────────────────┴───────────────────┘
```

### Combat Hazards
```
┌─────────────────────────────────────┐
│            COMBAT RISKS             │
├─────────────────┬───────────────────┤
│  THREAT         │  COUNTERMEASURE   │
├─────────────────┼───────────────────┤
│  Torpedo        │  Evasive maneuvers│
│  Missile         │  Chaff flares     │
│  Depth Charge    │  Deep dive        │
│  Sonar Ping      │  Silent running   │
│  Radar Lock      │  ECM              │
│  Homing Weapon   │  Decoy            │
│  Boarding Party  │  Security lock    │
└─────────────────┴───────────────────┘
```

---

## 🎮 Control Schemes Comparison

### Default Scheme
```
┌─────────────────────────────────────┐
│          DEFAULT CONTROLS          │
├─────────────────┬───────────────────┤
│  ACTION         │  KEY              │
├─────────────────┼───────────────────┤
│  Move           │  Arrow Keys       │
│  Turn           │  Arrow Keys       │
│  Accelerate     │  Arrow Keys       │
│  Periscope      │  P                │
│  Embark         │  E                │
│  Disembark      │  M (water)        │
│  Eject          │  TAB (air)        │
│  Torpedo        │  SPACE            │
│  Missile        │  ENTER            │
│  Depth Charge   │  SHIFT            │
│  Strafe         │  J/L (land)       │
│  Warp           │  ALT GR           │
│  Autopilot      │  1-9              │
└─────────────────┴───────────────────┘
```

### Alternative Scheme (Planned)
```
┌─────────────────────────────────────┐
│        ALTERNATIVE CONTROLS        │
├─────────────────┬───────────────────┤
│  ACTION         │  KEY              │
├─────────────────┼───────────────────┤
│  Move           │  WASD             │
│  Turn           │  Mouse            │
│  Accelerate     │  W/S              │
│  Periscope      │  Middle Mouse     │
│  Embark         │  F                │
│  Disembark      │  G (water)        │
│  Eject          │  C (air)          │
│  Torpedo        │  Left Mouse       │
│  Missile        │  Right Mouse      │
│  Depth Charge   │  X                │
│  Strafe         │  A/D (land)       │
│  Warp           │  Shift+W          │
│  Autopilot      │  F1-F9            │
└─────────────────┴───────────────────┘
```

---

## 🎓 Tips & Tricks

### Efficient Movement
- **Combine turns** with acceleration for tighter maneuvers
- **Use periscope mode** when near surface threats
- **Maintain speed** for quick space transition
- **Plan orbits** before planetary approach

### Combat Strategies
- **Lead targets** when firing torpedoes
- **Use terrain** for cover against sonar
- **Depth charges** work best in shallow water
- **Missiles** are heat-seeking - use flares

### Space Travel
- **88 mph minimum** for space transition
- **Sharp upward turn** required
- **Autopilot numbers** correspond to planetary order
- **Warp drive** consumes fuel quickly

### Emergency Procedures
- **TAB ejects** in air emergencies
- **M disembarks** underwater
- **E embarks** on land
- **P toggles** stealth modes

---

## 🔧 Control Customization (Future)

### Planned Features
- **Key binding** configuration
- **Controller support** (gamepad, joystick)
- **Sensitivity adjustment** for analog controls
- **Inverted controls** option
- **Vibration feedback** settings
- **Haptic feedback** integration

### Configuration File
```json
{
  "controls": {
    "movement": {
      "forward": "ArrowUp",
      "backward": "ArrowDown",
      "left": "ArrowLeft",
      "right": "ArrowRight"
    },
    "actions": {
      "periscope": "P",
      "embark": "E",
      "disembark": "M",
      "eject": "Tab",
      "torpedo": "Space",
      "missile": "Enter",
      "depth_charge": "Shift"
    },
    "sensitivity": 0.75,
    "invert_y": false,
    "vibration": true
  }
}
```

---

## 📊 Control Reference Cheat Sheet

```
┌─────────────────────────────────────────────────────┐
│           AFFINESCRIPT QUICK REFERENCE             │
├─────────────────────────────────────────────────────┤
│                                                     │
│  MOVEMENT:        Arrow Keys (← ↑ → ↓)              │
│  PERISCOPE:       P (toggle stealth)                │
│  EMBARK:          E (land/vehicle)                  │
│  DISEMBARK:       M (water) / TAB (air)            │
│  TORPEDO:         SPACE                             │
│  MISSILE:         ENTER                             │
│  DEPTH CHARGE:    SHIFT                             │
│  STRAFE:          J (left) L (right) - land only    │
│  SPACE:           88 mph + ↑ at screen top          │
│  AUTOPILOT:       1-9 (planets from sun)           │
│  WARP:            ALT GR (right alt)                │
│                                                     │
├─────────────────────────────────────────────────────┤
│  SPACE NAVIGATION:                                  │
│  1=Mercury, 2=Venus, 3=Earth, 4=Mars, 5=Jupiter     │
│  6=Saturn, 7=Uranus, 8=Neptune, 9=Pluto             │
│  E=Interact (Enter/Orbit/Depart)                    │
│  ALT GR=Warp/Galaxy Map                             │
│                                                     │
└─────────────────────────────────────────────────────┘
```

---

## 🎯 Future Control Enhancements

### Planned Additions
- **Gamepad Support**: Full controller mapping
- **Joystick Support**: Flight stick compatibility
- **Touch Controls**: Mobile device interface
- **Voice Commands**: Experimental voice control
- **Gesture Controls**: Motion sensing
- **Eye Tracking**: Gaze-based interaction
- **Haptic Feedback**: Enhanced tactile response
- **Adaptive Controls**: Context-sensitive bindings

### Roadmap
```
┌─────────────────────────────────────┐
│        CONTROL FEATURE ROADMAP      │
├─────────────────┬───────────────────┤
│  VERSION        │  FEATURES         │
├─────────────────┼───────────────────┤
│  Alpha-1        │  Basic controls   │
│                 │  Keyboard only    │
│                 │  Fixed bindings   │
├─────────────────┼───────────────────┤
│  Alpha-2        │  Key rebinding    │
│                 │  Gamepad support  │
│                 │  Sensitivity adj. │
├─────────────────┼───────────────────┤
│  Beta           │  Controller presets│
│                 │  Touch controls   │
│                 │  Haptic feedback  │
├─────────────────┼───────────────────┤
│  1.0            │  Voice commands   │
│                 │  Gesture controls │
│                 │  Eye tracking     │
│                 │  Adaptive controls│
└─────────────────┴───────────────────┘
```

---

## 🔒 Summary

**Current Controls:**
- Arrow keys for movement
- P for periscope/stealth
- E/M for embark/disembark
- SPACE/ENTER/SHIFT for weapons
- 88 mph + ↑ for space transition
- 1-9 for planetary autopilot
- ALT GR for warp speed

**Key Principle:** Simple, intuitive controls with clear separation between movement and action keys.

**Future:** Expand to full controller support, customization, and alternative input methods.

---

**Last Updated:** March 31, 2026
**Version:** Alpha-1
**Status:** Basic controls implemented, combat system planned

SPDX-License-Identifier: AGPL-3.0-or-later
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell