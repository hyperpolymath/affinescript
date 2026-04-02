# AffineScript Submarine Damage System

## 🚨 Critical Damage Information

**For Players and AI Systems:** This document explains how submarine damage affects gameplay and what happens when different parts of your submarine are damaged.

---

## 🛡️ Submarine Component Damage Effects

### Visual Damage Indicators
```
┌─────────────────────────────────────────────────────┐
│           SUBMARINE DAMAGE INDICATORS             │
├─────────────────────────────────────────────────────┤
│                                                     │
│  FRONT SHIELD:       [■■■■■■■■■■] 100%            │
│  DAMAGE METER:       [□□□□□□□□□□] 0%              │
│                                                     │
│  █████████████████████████████████████████████████  │
│  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  │
│                                                     │
│  [FRONT] [WINGS] [MIDDLE] [BACK] [PERISCOPE]      │
│                                                     │
└─────────────────────────────────────────────────────┘
```

---

## 🔧 Damage System Breakdown

### 1. **Front Shield Damage**
**Indicator:** Shield integrity meter at top of HUD

**Effects:**
```
┌─────────────────────────────────────┐
│          FRONT SHIELD DAMAGE        │
├─────────────────┬───────────────────┤
│  DAMAGE LEVEL   │  EFFECT           │
├─────────────────┼───────────────────┤
│  0-20%          │  Minor handling    │
│                 │  issues           │
├─────────────────┼───────────────────┤
│  21-50%         │  Moderate control │
│                 │  erraticism      │
├─────────────────┼───────────────────┤
│  51-80%         │  Severe handling  │
│                 │  difficulties    │
├─────────────────┼───────────────────┤
│  81-100%        │  Critical failure │
│                 │  - Front end     │
│                 │    takes direct  │
│                 │    damage        │
│                 │  - Controls      │
│                 │    become highly │
│                 │    erratic       │
│                 │  - Turning       │
│                 │    radius        │
│                 │    increases     │
│                 │  - Response time │
│                 │    degraded       │
└─────────────────┴───────────────────┘
```

**Gameplay Impact:**
- Submarine becomes harder to control
- Turning requires more anticipation
- Precision maneuvers become difficult
- **Front end takes direct damage after shield failure**

---

### 2. **Front End Damage** (After Shield Failure)
**Indicator:** Red flashing on front section of submarine

**Effects:**
```
┌─────────────────────────────────────┐
│          FRONT END DAMAGE           │
├─────────────────┬───────────────────┤
│  DAMAGE LEVEL   │  EFFECT           │
├─────────────────┼───────────────────┤
│  Any damage     │  Controls become │
│                 │  erratic         │
├─────────────────┼───────────────────┤
│  30%+           │  - Turning circle │
│                 │    increases 50% │
│                 │  - Response lag   │
│                 │    +200ms        │
│                 │  - Maximum speed  │
│                 │    reduced 15%   │
├─────────────────┼───────────────────┤
│  60%+           │  - Turning circle │
│                 │    increases 100%│
│                 │  - Response lag   │
│                 │    +500ms        │
│                 │  - Maximum speed  │
│                 │    reduced 30%   │
│                 │  - Random yaws   │
│                 │    during turns  │
├─────────────────┼───────────────────┤
│  90%+           │  - Controls      │
│                 │    barely        │
│                 │    responsive    │
│                 │  - Submarine     │
│                 │    drifts        │
│                 │    unpredictably │
│                 │  - Emergency     │
│                 │    surfacing     │
│                 │    recommended   │
└─────────────────┴───────────────────┘
```

**AI Considerations:**
- Increase prediction algorithms for turning
- Reduce maximum speed calculations
- Add randomness factor to movement
- Implement emergency surfacing protocol

---

### 3. **Wing Damage**
**Indicator:** Damage indicators on port/starboard sides

**Effects:**
```
┌─────────────────────────────────────┐
│           WING DAMAGE               │
├─────────────────┬───────────────────┤
│  DAMAGE LEVEL   │  EFFECT           │
├─────────────────┼───────────────────┤
│  0-30%          │  Reduced climb    │
│                 │  rate             │
├─────────────────┼───────────────────┤
│  31-60%         │  - Climb rate    │
│                 │    -40%          │
│                 │  - Turn radius   │
│                 │    +30%         │
│                 │  - Banking       │
│                 │    less precise  │
├─────────────────┼───────────────────┤
│  61-90%         │  - Climb rate    │
│                 │    -70%          │
│                 │  - Turn radius   │
│                 │    +60%         │
│                 │  - Severe roll   │
│                 │    instability   │
│                 │  - Risk of       │
│                 │    uncontrolled  │
│                 │    spin          │
├─────────────────┼───────────────────┤
│  91-100%        │  - WINGS         │
│                 │    DESTROYED     │
│                 │  - NO FLIGHT     │
│                 │    CAPABILITY    │
│                 │  - Submarine     │
│                 │    sinks         │
│                 │  - Emergency      │
│                 │    ballast       │
│                 │    required      │
└─────────────────┴───────────────────┘
```

**Gameplay Impact:**
- Reduced maneuverability in air
- Difficulty maintaining altitude
- Increased stall risk
- **Complete wing destruction = no flight capability**

**AI Considerations:**
- Reduce maximum bank angle
- Increase stall speed calculations
- Add roll damping compensation
- Implement emergency ballast procedures

---

### 4. **Back End Damage**
**Indicator:** Damage indicators on rear section

**Effects:**
```
┌─────────────────────────────────────┐
│           BACK END DAMAGE           │
├─────────────────┬───────────────────┤
│  DAMAGE LEVEL   │  EFFECT           │
├─────────────────┼───────────────────┤
│  0-25%          │  Reduced speed    │
├─────────────────┼───────────────────┤
│  26-50%         │  - Maximum speed │
│                 │    -20%          │
│                 │  - Acceleration  │
│                 │    -15%         │
│                 │  - Fuel          │
│                 │    efficiency    │
│                 │    -10%         │
├─────────────────┼───────────────────┤
│  51-75%         │  - Maximum speed │
│                 │    -40%          │
│                 │  - Acceleration  │
│                 │    -30%         │
│                 │  - Fuel          │
│                 │    efficiency    │
│                 │    -25%         │
│                 │  - Maneuvering  │
│                 │    thrusters     │
│                 │    -50%         │
├─────────────────┼───────────────────┤
│  76-100%        │  - Maximum speed │
│                 │    -60%          │
│                 │  - Acceleration  │
│                 │    -50%         │
│                 │  - Fuel          │
│                 │    efficiency    │
│                 │    -40%         │
│                 │  - Maneuvering  │
│                 │    thrusters     │
│                 │    -80%         │
│                 │  - Reverse       │
│                 │    capability    │
│                 │    lost          │
└─────────────────┴───────────────────┘
```

**Gameplay Impact:**
- Slower movement in water and air
- Reduced acceleration
- Poor fuel economy
- Difficulty in precise maneuvering
- **Severe damage prevents reverse movement**

**AI Considerations:**
- Reduce maximum speed parameters
- Increase fuel consumption calculations
- Add thrust vector randomization
- Implement emergency propulsion protocols

---

### 5. **Middle Section Damage**
**Indicator:** Central damage warning

**Effects:**
```
┌─────────────────────────────────────┐
│          MIDDLE SECTION DAMAGE     │
├─────────────────┬───────────────────┤
│  DAMAGE LEVEL   │  EFFECT           │
├─────────────────┼───────────────────┤
│  0-20%          │  Reduced          │
│                 │  buoyancy         │
├─────────────────┼───────────────────┤
│  21-40%         │  - Buoyancy       │
│                 │    -15%          │
│                 │  - Sink rate      │
│                 │    +10%         │
│                 │  - Stability      │
│                 │    -5%          │
├─────────────────┼───────────────────┤
│  41-60%         │  - Buoyancy       │
│                 │    -30%          │
│                 │  - Sink rate      │
│                 │    +25%         │
│                 │  - Stability      │
│                 │    -15%         │
│                 │  - PERISCOPE      │
│                 │    MODE          │
│                 │    DISABLED      │
├─────────────────┼───────────────────┤
│  61-80%         │  - Buoyancy       │
│                 │    -50%          │
│                 │  - Sink rate      │
│                 │    +50%         │
│                 │  - Stability      │
│                 │    -30%         │
│                 │  - PERISCOPE      │
│                 │    MODE          │
│                 │    DISABLED      │
│                 │  - Constant       │
│                 │    downward       │
│                 │    drift          │
├─────────────────┼───────────────────┤
│  81-100%        │  - Buoyancy       │
│                 │    -75%          │
│                 │  - Sink rate      │
│                 │    +100%        │
│                 │  - Stability      │
│                 │    -50%         │
│                 │  - PERISCOPE      │
│                 │    MODE          │
│                 │    DISABLED      │
│                 │  - Severe         │
│                 │    downward       │
│                 │    pull           │
│                 │  - SPACE FLIGHT   │
│                 │    IMPOSSIBLE     │
│                 │  - Emergency      │
│                 │    ballast       │
│                 │    dump           │
│                 │    required      │
└─────────────────┴───────────────────┘
```

**Gameplay Impact:**
- Submarine sinks faster
- Difficulty maintaining depth
- Periscope mode unavailable
- **Space flight impossible** until repaired
- Constant downward drift

**AI Considerations:**
- Increase ballast calculations
- Add constant downward velocity
- Disable periscope functionality
- Block space flight attempts
- Implement emergency ballast dump

---

## 🔧 Damage Progression System

### Damage Accumulation
```
FRONT SHIELD (100%) → FRONT END → WINGS → BACK → MIDDLE
```

### Repair Priorities
```
1. MIDDLE SECTION (restores space flight, buoyancy)
2. FRONT SHIELD (prevents front end damage)
3. WINGS (restores flight capability)
4. BACK END (improves speed)
5. FRONT END (improves control)
```

---

## 🎮 Player Strategies for Damaged Submarines

### Front Shield Damaged
```markdown
✅ Use gentle, anticipatory turns
✅ Reduce speed to maintain control
✅ Avoid rapid direction changes
✅ Plan wider turning radius
✅ Consider emergency repairs
```

### Front End Damaged
```markdown
✅ Reduce speed by 30-50%
✅ Use small, gradual corrections
✅ Avoid tight spaces
✅ Plan turns well in advance
✅ Consider surfacing if severe
```

### Wings Damaged
```markdown
✅ Avoid sharp bank turns
✅ Maintain level flight
✅ Reduce altitude changes
✅ Use gentle climbs/descents
✅ Prepare for emergency landing
```

### Back End Damaged
```markdown
✅ Accept reduced speed
✅ Plan longer travel times
✅ Avoid situations requiring reverse
✅ Monitor fuel consumption
✅ Consider conservative routes
```

### Middle Section Damaged
```markdown
✅ Activate emergency ballast
✅ Seek shallow waters
✅ Avoid deep dives
✅ Prioritize repairs
✅ Space flight impossible
```

---

## 🤖 AI System Considerations

### Damage Detection
```python
# Pseudocode for AI damage detection
def detect_damage(submarine):
    if submarine.front_shield < 100:
        # Front shield damaged
        increase_turn_prediction()
        add_control_randomness()
    
    if submarine.front_damage > 0:
        # Front end damaged
        reduce_max_speed(0.3)
        increase_turn_radius(0.5)
        add_response_lag(200)
    
    if submarine.wing_damage > 30:
        # Wings damaged
        reduce_climb_rate(0.4)
        increase_turn_radius(0.3)
        add_roll_instability()
    
    if submarine.back_damage > 25:
        # Back end damaged
        reduce_max_speed(0.2)
        reduce_acceleration(0.15)
        reduce_fuel_efficiency(0.1)
    
    if submarine.middle_damage > 0:
        # Middle damaged
        reduce_buoyancy(0.15)
        increase_sink_rate(0.1)
        disable_periscope()
        disable_space_flight()
        
    if submarine.middle_damage > 40:
        add_constant_downward_drift()
```

### AI Behavior Adjustments
```python
def adjust_ai_behavior(submarine):
    # Front damage adjustments
    if submarine.front_damage > 50:
        submarine.ai.turn_prediction += 0.5
        submarine.ai.response_lag = 0.3
        submarine.ai.max_bank_angle = 20
    
    # Wing damage adjustments
    if submarine.wing_damage > 60:
        submarine.ai.max_climb_angle = 15
        submarine.ai.stall_speed += 10
        submarine.ai.roll_damping = 0.7
    
    # Back damage adjustments
    if submarine.back_damage > 50:
        submarine.ai.max_speed = 0.6
        submarine.ai.acceleration = 0.7
        submarine.ai.fuel_efficiency = 0.6
    
    # Middle damage adjustments
    if submarine.middle_damage > 20:
        submarine.ai.buoyancy = 0.85
        submarine.ai.sink_rate = 1.1
        submarine.ai.can_use_periscope = False
        submarine.ai.can_space_flight = False
        
        if submarine.middle_damage > 60:
            submarine.ai.constant_sink = 0.5  # units per second
```

---

## 🛠️ Repair Systems

### Emergency Repairs
```
┌─────────────────────────────────────┐
│          EMERGENCY REPAIRS         │
├─────────────────┬───────────────────┤
│  COMPONENT      │  METHOD           │
├─────────────────┼───────────────────┤
│  Front Shield   │  Shield Generator │
│                 │  (50% capacity)   │
├─────────────────┼───────────────────┤
│  Front End      │  Hull Patching    │
│                 │  (Temporary)      │
├─────────────────┼───────────────────┤
│  Wings          │  Structural Brace │
│                 │  (Reduces flight  │
│                 │   capability)    │
├─────────────────┼───────────────────┤
│  Back End       │  Engine Bypass    │
│                 │  (50% thrust)    │
├─────────────────┼───────────────────┤
│  Middle         │  Ballast Adjust   │
│                 │  (Temporary)      │
│                 │  Emergency Pump  │
└─────────────────┴───────────────────┘
```

### Full Repairs
```
┌─────────────────────────────────────┐
│           FULL REPAIRS              │
├─────────────────┬───────────────────┤
│  COMPONENT      │  REQUIREMENTS    │
├─────────────────┼───────────────────┤
│  Front Shield   │  Dry Dock        │
│                 │  200 Steel        │
│                 │  100 Energy       │
├─────────────────┼───────────────────┤
│  Front End      │  Dry Dock        │
│                 │  300 Steel        │
│                 │  150 Energy       │
├─────────────────┼───────────────────┤
│  Wings          │  Dry Dock        │
│                 │  400 Steel        │
│                 │  200 Energy       │
├─────────────────┼───────────────────┤
│  Back End       │  Dry Dock        │
│                 │  350 Steel        │
│                 │  175 Energy       │
├─────────────────┼───────────────────┤
│  Middle         │  Dry Dock        │
│                 │  500 Steel        │
│                 │  250 Energy       │
│                 │  Periscope Module │
│                 │  Space Flight     │
│                 │  Certification   │
└─────────────────┴───────────────────┘
```

---

## ⚠️ Critical Damage States

### Imminent Failure Warnings
```
┌─────────────────────────────────────┐
│         CRITICAL DAMAGE STATES      │
├─────────────────┬───────────────────┤
│  COMPONENT      │  WARNING          │
├─────────────────┼───────────────────┤
│  Front Shield   │  "SHIELD CRITICAL"│
│  <20%           │  "FRONT EXPOSED"  │
├─────────────────┼───────────────────┤
│  Front End      │  "CONTROLS FAILING"│
│  >80%           │  "EMERGENCY SURFACE"│
├─────────────────┼───────────────────┤
│  Wings          │  "FLIGHT CRITICAL"│
│  >90%           │  "PREPARE FOR     │
│                 │   IMPACT"         │
├─────────────────┼───────────────────┤
│  Back End       │  "PROPULSION      │
│  >75%           │   FAILURE"        │
│                 │  "STRANDED RISK" │
├─────────────────┼───────────────────┤
│  Middle         │  "SINKING IMMINENT"│
│  >80%           │  "ABANDON SHIP"   │
│                 │  "SPACE FLIGHT    │
│                 │   IMPOSSIBLE"    │
└─────────────────┴───────────────────┘
```

### AI Emergency Protocols
```python
def critical_damage_response(submarine):
    if submarine.front_shield < 20:
        # Front shield critical
        submarine.ai.avoid_combat()
        submarine.ai.seek_repair()
        submarine.ai.reduce_speed(0.5)
        
    if submarine.front_damage > 80:
        # Front end critical
        submarine.ai.emergency_surface()
        submarine.ai.avoid_all_combat()
        submarine.ai.signal_distress()
        
    if submarine.wing_damage > 90:
        # Wings critical
        submarine.ai.prepare_for_crash()
        submarine.ai.deploy_parachute()
        submarine.ai.eject_crew()
        
    if submarine.back_damage > 75:
        # Back end critical
        submarine.ai.anchor_submarine()
        submarine.ai.request_tow()
        submarine.ai.activate_beacon()
        
    if submarine.middle_damage > 80:
        # Middle critical
        submarine.ai.abandon_ship()
        submarine.ai.deploy_lifeboats()
        submarine.ai.transmit_distress()
```

---

## 📊 Damage Effect Summary Table

```
┌─────────────────────────────────────────────────────────────┐
│              SUBMARINE DAMAGE EFFECTS SUMMARY              │
├─────────────────┬───────────────────┬───────────────────────┤
│  COMPONENT      │  DAMAGE LEVEL     │  PRIMARY EFFECTS      │
├─────────────────┼───────────────────┼───────────────────────┤
│  Front Shield   │  0-100%           │ Controls become      │
│                 │                   │ erratic when failed  │
├─────────────────┼───────────────────┼───────────────────────┤
│  Front End      │  Any damage       │ - Erratic controls   │
│                 │                   │ - Increased turn     │
│                 │                   │   radius             │
│                 │                   │ - Reduced max speed  │
├─────────────────┬───────────────────┼───────────────────────┤
│  Wings          │  30%+             │ - Reduced climb rate │
│                 │                   │ - Increased turn     │
│                 │                   │   radius            │
│                 │                   │ - Roll instability  │
│                 │                   │ - 90%+ = NO FLIGHT  │
├─────────────────┬───────────────────┼───────────────────────┤
│  Back End       │  25%+             │ - Reduced max speed │
│                 │                   │ - Reduced           │
│                 │                   │   acceleration      │
│                 │                   │ - Poor fuel         │
│                 │                   │   efficiency       │
│                 │                   │ - 75%+ = NO REVERSE │
├─────────────────┬───────────────────┼───────────────────────┤
│  Middle         │  Any damage       │ - Reduced buoyancy  │
│                 │                   │ - Increased sink    │
│                 │                   │   rate             │
│                 │                   │ - Periscope        │
│                 │                   │   DISABLED         │
│                 │                   │ - 20%+ = Constant  │
│                 │                   │   downward drift   │
│                 │                   │ - SPACE FLIGHT    │
│                 │                   │   IMPOSSIBLE      │
└─────────────────┴───────────────────┴───────────────────────┘
```

---

## 🎯 Player Decision Flowchart

```
IS FRONT SHIELD DAMAGED?
   │
   ├── YES → Avoid combat, seek repair
   │       → Reduce speed, gentle turns
   │
   └── NO → Proceed normally
           │
           ├── IS FRONT END DAMAGED?
           │   │
           │   ├── YES → Reduce speed 50%
           │   │    → Avoid tight turns
           │   │    → Consider surfacing
           │   │
           │   └── NO → Continue
           │        │
           │        ├── ARE WINGS DAMAGED?
           │        │   │
           │        │   ├── YES → Avoid sharp turns
           │        │   │    → Maintain level flight
           │        │   │    → Prepare for landing
           │        │   │
           │        │   └── NO → Continue
           │        │        │
           │        │        ├── IS BACK END DAMAGED?
           │        │        │   │
           │        │        │   ├── YES → Accept reduced speed
           │        │        │   │    → Monitor fuel
           │        │        │   │    → Avoid reverse
           │        │        │   │
           │        │        │   └── NO → Continue
           │        │        │        │
           │        │        │        ├── IS MIDDLE DAMAGED?
           │        │        │        │   │
           │        │        │        │   ├── YES → Activate emergency ballast
           │        │        │        │   │    → Seek shallow water
           │        │        │        │   │    → Prioritize repairs
           │        │        │        │   │    → SPACE FLIGHT IMPOSSIBLE
           │        │        │        │   │
           │        │        │        │   └── NO → Full capability
           │        │        │        │
           │        │        │        └── All systems operational
           │        │        │
           │        │        └── Optimal performance
           │        │
           │        └── Normal operations
           │
           └── Standard procedures

```

---

## 🤖 AI Damage Response Matrix

```
┌─────────────────────────────────────────────────────────────┐
│              AI DAMAGE RESPONSE PROTOCOLS                  │
├─────────────────┬───────────────────┬───────────────────────┤
│  COMPONENT      │  DAMAGE LEVEL     │  AI RESPONSE         │
├─────────────────┼───────────────────┼───────────────────────┤
│  Front Shield   │  <50%             │ Increase turn       │
│                 │                   │ prediction by 25%   │
├─────────────────┼───────────────────┼───────────────────────┤
│  Front Shield   │  50-80%           │ Increase turn       │
│                 │                   │ prediction by 50%   │
│                 │                   │ Add 10% control     │
│                 │                   │ randomness          │
├─────────────────┼───────────────────┼───────────────────────┤
│  Front Shield   │  >80%             │ Increase turn       │
│                 │                   │ prediction by 75%   │
│                 │                   │ Add 25% control     │
│                 │                   │ randomness          │
│                 │                   │ Reduce max speed    │
│                 │                   │ by 30%             │
├─────────────────┼───────────────────┼───────────────────────┤
│  Front End      │  20-50%           │ Reduce max speed    │
│                 │                   │ by 20%             │
│                 │                   │ Increase turn      │
│                 │                   │ radius by 30%      │
├─────────────────┼───────────────────┼───────────────────────┤
│  Front End      │  51-80%           │ Reduce max speed    │
│                 │                   │ by 40%             │
│                 │                   │ Increase turn      │
│                 │                   │ radius by 60%      │
│                 │                   │ Add 15% yaw        │
│                 │                   │ instability        │
├─────────────────┼───────────────────┼───────────────────────┤
│  Front End      │  >80%             │ Reduce max speed    │
│                 │                   │ by 60%             │
│                 │                   │ Increase turn      │
│                 │                   │ radius by 100%     │
│                 │                   │ Add 30% yaw        │
│                 │                   │ instability        │
│                 │                   │ Add 500ms         │
│                 │                   │ response lag      │
├─────────────────┼───────────────────┼───────────────────────┤
│  Wings          │  30-60%           │ Reduce climb rate  │
│                 │                   │ by 40%            │
│                 │                   │ Increase turn     │
│                 │                   │ radius by 30%     │
├─────────────────┼───────────────────┼───────────────────────┤
│  Wings          │  61-90%           │ Reduce climb rate  │
│                 │                   │ by 70%            │
│                 │                   │ Increase turn     │
│                 │                   │ radius by 60%     │
│                 │                   │ Add roll          │
│                 │                   │ instability       │
├─────────────────┼───────────────────┼───────────────────────┤
│  Wings          │  >90%             │ DISABLE FLIGHT    │
│                 │                   │ Prepare for       │
│                 │                   │ impact            │
│                 │                   │ Deploy            │
│                 │                   │ parachute        │
├─────────────────┼───────────────────┼───────────────────┤
│  Back End       │  25-50%           │ Reduce max speed  │
│                 │                   │ by 20%           │
│                 │                   │ Reduce           │
│                 │                   │ acceleration by  │
│                 │                   │ 15%             │
├─────────────────┼───────────────────┼───────────────────┤
│  Back End       │  51-75%           │ Reduce max speed  │
│                 │                   │ by 40%           │
│                 │                   │ Reduce           │
│                 │                   │ acceleration by  │
│                 │                   │ 30%             │
│                 │                   │ Reduce fuel      │
│                 │                   │ efficiency by    │
│                 │                   │ 25%             │
├─────────────────┼───────────────────┼───────────────────┤
│  Back End       │  >75%             │ Reduce max speed  │
│                 │                   │ by 60%           │
│                 │                   │ Reduce           │
│                 │                   │ acceleration by  │
│                 │                   │ 50%             │
│                 │                   │ DISABLE REVERSE  │
├─────────────────┼───────────────────┼───────────────────┤
│  Middle         │  0-20%            │ Reduce buoyancy  │
│                 │                   │ by 15%           │
│                 │                   │ Increase sink    │
│                 │                   │ rate by 10%      │
│                 │                   │ DISABLE         │
│                 │                   │ PERISCOPE       │
├─────────────────┼───────────────────┼───────────────────┤
│  Middle         │  21-40%           │ Reduce buoyancy  │
│                 │                   │ by 30%           │
│                 │                   │ Increase sink    │
│                 │                   │ rate by 25%      │
│                 │                   │ DISABLE         │
│                 │                   │ PERISCOPE       │
├─────────────────┼───────────────────┼───────────────────┤
│  Middle         │  41-60%           │ Reduce buoyancy  │
│                 │                   │ by 50%           │
│                 │                   │ Increase sink    │
│                 │                   │ rate by 50%      │
│                 │                   │ DISABLE         │
│                 │                   │ PERISCOPE       │
│                 │                   │ Add constant    │
│                 │                   │ downward drift  │
├─────────────────┼───────────────────┼───────────────────┤
│  Middle         │  61-80%           │ Reduce buoyancy  │
│                 │                   │ by 75%           │
│                 │                   │ Increase sink    │
│                 │                   │ rate by 100%     │
│                 │                   │ DISABLE         │
│                 │                   │ PERISCOPE       │
│                 │                   │ DISABLE SPACE   │
│                 │                   │ FLIGHT          │
│                 │                   │ Add severe      │
│                 │                   │ downward drift  │
├─────────────────┼───────────────────┼───────────────────┤
│  Middle         │  >80%             │ EMERGENCY       │
│                 │                   │ PROTOCOLS       │
│                 │                   │ Prepare         │
│                 │                   │ abandon ship    │
│                 │                   │ Signal distress │
│                 │                   │ Activate        │
│                 │                   │ lifeboats       │
└─────────────────┴───────────────────┴───────────────────┘
```

---

## 🎓 Player Tips for Managing Damage

### Preventive Measures
```markdown
✅ Maintain front shield integrity
✅ Avoid unnecessary combat
✅ Use terrain for cover
✅ Monitor damage indicators
✅ Repair at first opportunity
```

### Emergency Procedures
```markdown
⚠️ Front shield critical → Seek immediate repair
⚠️ Front end damaged → Reduce speed, avoid turns
⚠️ Wings damaged → Maintain level flight
⚠️ Back end damaged → Accept reduced speed
⚠️ Middle damaged → Activate emergency ballast
```

### Repair Strategies
```markdown
🔧 Prioritize middle section repairs
🔧 Restore front shield before combat
🔧 Fix wings for flight capability
🔧 Repair back end for speed
🔧 Full dry dock for complete restoration
```

---

## 🤖 AI-Specific Damage Handling

### Damage State Machine
```
IDLE → DETECT DAMAGE → ASSESS SEVERITY → ACTIVATE PROTOCOLS → MONITOR → REASSESS
```

### AI Damage Response Code
```python
class SubmarineAIDamageHandler:
    def __init__(self, submarine):
        self.submarine = submarine
        self.damage_protocols = {
            'front_shield': self.handle_front_shield,
            'front_end': self.handle_front_end,
            'wings': self.handle_wings,
            'back_end': self.handle_back_end,
            'middle': self.handle_middle
        }
    
    def update(self):
        for component, handler in self.damage_protocols.items():
            damage = getattr(self.submarine.damage, component)
            if damage > 0:
                handler(damage)
    
    def handle_front_shield(self, damage_level):
        if damage_level < 50:
            self.submarine.ai.turn_prediction *= 1.25
        elif damage_level < 80:
            self.submarine.ai.turn_prediction *= 1.5
            self.submarine.ai.control_randomness = 0.1
        else:
            self.submarine.ai.turn_prediction *= 1.75
            self.submarine.ai.control_randomness = 0.25
            self.submarine.ai.max_speed *= 0.7
    
    def handle_front_end(self, damage_level):
        if damage_level < 50:
            self.submarine.ai.max_speed *= 0.8
            self.submarine.ai.turn_radius *= 1.3
        elif damage_level < 80:
            self.submarine.ai.max_speed *= 0.6
            self.submarine.ai.turn_radius *= 1.6
            self.submarine.ai.yaw_instability = 0.15
        else:
            self.submarine.ai.max_speed *= 0.4
            self.submarine.ai.turn_radius *= 2.0
            self.submarine.ai.yaw_instability = 0.3
            self.submarine.ai.response_lag = 0.5
    
    def handle_wings(self, damage_level):
        if damage_level < 60:
            self.submarine.ai.climb_rate *= 0.6
            self.submarine.ai.turn_radius *= 1.3
        elif damage_level < 90:
            self.submarine.ai.climb_rate *= 0.3
            self.submarine.ai.turn_radius *= 1.6
            self.submarine.ai.roll_instability = 0.2
        else:
            self.submarine.ai.can_fly = False
            self.submarine.ai.prepare_for_impact()
    
    def handle_back_end(self, damage_level):
        if damage_level < 50:
            self.submarine.ai.max_speed *= 0.8
            self.submarine.ai.acceleration *= 0.85
            self.submarine.ai.fuel_efficiency *= 0.9
        elif damage_level < 75:
            self.submarine.ai.max_speed *= 0.6
            self.submarine.ai.acceleration *= 0.7
            self.submarine.ai.fuel_efficiency *= 0.75
        else:
            self.submarine.ai.max_speed *= 0.4
            self.submarine.ai.acceleration *= 0.5
            self.submarine.ai.fuel_efficiency *= 0.6
            self.submarine.ai.can_reverse = False
    
    def handle_middle(self, damage_level):
        if damage_level < 20:
            self.submarine.ai.buoyancy *= 0.85
            self.submarine.ai.sink_rate *= 1.1
            self.submarine.ai.can_use_periscope = False
        elif damage_level < 40:
            self.submarine.ai.buoyancy *= 0.7
            self.submarine.ai.sink_rate *= 1.25
            self.submarine.ai.can_use_periscope = False
        elif damage_level < 60:
            self.submarine.ai.buoyancy *= 0.5
            self.submarine.ai.sink_rate *= 1.5
            self.submarine.ai.can_use_periscope = False
            self.submarine.ai.constant_sink = 0.3
        elif damage_level < 80:
            self.submarine.ai.buoyancy *= 0.25
            self.submarine.ai.sink_rate *= 2.0
            self.submarine.ai.can_use_periscope = False
            self.submarine.ai.can_space_flight = False
            self.submarine.ai.constant_sink = 0.7
        else:
            self.submarine.ai.abandon_ship()
            self.submarine.ai.signal_distress()
            self.submarine.ai.activate_lifeboats()
```

---

## 🔍 Damage Assessment Checklist

### For Players
```markdown
[ ] Check front shield integrity
[ ] Assess front end damage
[ ] Inspect wing condition
[ ] Evaluate back end status
[ ] Examine middle section
[ ] Test periscope functionality
[ ] Verify space flight capability
[ ] Monitor buoyancy
[ ] Check fuel efficiency
[ ] Test maneuverability
```

### For AI Systems
```markdown
[ ] Query front shield status
[ ] Retrieve front end damage level
[ ] Get wing damage percentage
[ ] Check back end functionality
[ ] Assess middle section integrity
[ ] Test periscope operation
[ ] Verify space flight systems
[ ] Calculate current buoyancy
[ ] Monitor fuel consumption
[ ] Evaluate control responsiveness
```

---

## 📊 Damage Impact Summary

```
┌─────────────────────────────────────────────────────────────────┐
│                    DAMAGE IMPACT QUICK REFERENCE                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                     │
│  FRONT SHIELD FAILURE → Front end takes direct damage            │
│                                                                     │
│  FRONT END DAMAGE → Erratic controls, wider turns, slower speed │
│                                                                     │
│  WING DAMAGE → Reduced climb, wider turns, roll instability     │
│                    90%+ = NO FLIGHT CAPABILITY                   │
│                                                                     │
│  BACK END DAMAGE → Slower speed, poor acceleration, no reverse  │
│                                                                     │
│  MIDDLE DAMAGE → Sinking, no periscope, NO SPACE FLIGHT         │
│                    Constant downward drift at higher damage       │
│                                                                     │
├─────────────────────────────────────────────────────────────────┤
│                    REPAIR PRIORITIES                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                     │
│  1. MIDDLE SECTION → Restores buoyancy & space flight            │
│                                                                     │
│  2. FRONT SHIELD → Prevents front end damage                    │
│                                                                     │
│  3. WINGS → Restores flight capability                           │
│                                                                     │
│  4. BACK END → Improves speed and acceleration                   │
│                                                                     │
│  5. FRONT END → Improves control and maneuverability             │
│                                                                     │
└─────────────────────────────────────────────────────────────────┘
```

---

## 🎯 Key Takeaways

### For Human Players
1. **Front shield is critical** - protect it to avoid front end damage
2. **Middle section damage is most severe** - prioritize its repair
3. **Wing damage prevents flight** - repair wings for air capability
4. **Back end damage slows you down** - affects speed and fuel efficiency
5. **Middle damage prevents space flight** - must be repaired for interplanetary travel

### For AI Systems
1. **Monitor front shield integrity** - adjust controls when damaged
2. **Detect middle section damage** - disable space flight systems
3. **Assess wing damage levels** - ground aircraft if wings critically damaged
4. **Calculate buoyancy changes** - compensate for sinking with ballast
5. **Implement emergency protocols** - abandon ship when middle damage >80%

### Universal Rules
- **Front shield failure exposes front end to damage**
- **Wing damage >90% = no flight capability**
- **Middle damage = no periscope, no space flight, increased sinking**
- **Back end damage >75% = no reverse capability**
- **All damage is repairable with proper resources**

---

## 🔒 Conclusion

The AffineScript submarine damage system creates **realistic, component-based damage** that affects gameplay in **specific, predictable ways**. Understanding this system allows both **human players** and **AI systems** to make informed decisions about **repair priorities**, **tactical maneuvers**, and **emergency procedures**.

**Remember:**
- Protect your front shield
- Prioritize middle section repairs
- Wing damage grounds aircraft
- Middle damage prevents space flight
- All damage can be repaired

**For AI Developers:** The damage system provides clear, quantifiable effects that can be integrated into pathfinding, combat, and emergency response algorithms.

**For Players:** Learn the damage effects to survive longer and make better repair decisions.

---

**Last Updated:** March 31, 2026
**Version:** Alpha-1
**Document Status:** Complete damage system specification

SPDX-License-Identifier: AGPL-3.0-or-later
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell and contributors