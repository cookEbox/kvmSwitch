# KvmSwitch LED control

KvmSwitch uses the native `rpi_ws281x` C library (`libws2811`) for all LED
transmission. Nix builds the pinned C library from `nix/rpi-ws281x.nix` and
supplies its headers, static archive, and `libws2811.pc` metadata to Cabal.
No Python package or Python runtime is required.

The public Haskell API is provided by `NeoPixel`:

```haskell
withPixelStrip, setPixel, showPixels, renderFrame
clearPixels, setBrightness, pixelCount
```

The configured strip uses 36 RGB pixels, BCM GPIO10 (SPI0 MOSI), GRB colour
ordering, and brightness 32/255. The native driver provides the 800 kHz
transmission timing. `setPixel` changes the buffered native frame without
transmitting; `showPixels`, `renderFrame`, and `clearPixels` transmit and
return `Either LedError`. Native failures include the original driver error
code and description.

The opt-in hardware tests are:

```bash
kvmSwitch --led-self-test
kvmSwitch --led-walking-test
```

Before running on a Pi, verify the model, SPI device, permissions, suitable
LED power, and common ground:

```bash
tr -d '\0' < /proc/device-tree/model
ls -l /dev/spidev0.0
```

SPI must already be enabled in the Pi configuration; the application does not
modify boot configuration. The normal test command is:

```bash
kvmSwitch --led-self-test
```

## Keyboard GPIO assignments and future SPI conflict

The isolated keyboard monitor uses these BCM GPIO assignments:

| Line | BCM GPIO |
| --- | ---: |
| R1 | 4 |
| R2 | 27 |
| R3 | 9 |
| R4 | 5 |
| R5 | 13 |
| C1 | 17 |
| C2 | 22 |
| C3 | 11 |
| C4 | 6 |
| C5 | 19 |
| C6 | 26 |
| C7 | 14 |
| C8 | 15 |

The repository source configuration is `config/keyboard.yaml`. Copy it to the
Pi's XDG configuration directory (the deployment helper is intentionally not
changed in this stage):

```bash
config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/kvm"
mkdir -p "$config_dir"
cp config/keyboard.yaml "$config_dir/keyboard.yaml"
```

Run the raw monitor with either the default path or an explicit path:

```bash
kvmSwitch --key-monitor
kvmSwitch --key-monitor /path/to/keyboard.yaml
```

It prints row and column BCM GPIO numbers for debounced press events. It does
not use the NeoPixel driver or interpret logical key mappings. The scanner
currently reports at most one detected key per scan and does not implement
multi-key or ghosting handling.

The NeoPixel driver uses SPI0, including BCM GPIO10 for MOSI. GPIO9 (SPI0 MISO)
and GPIO11 (SPI0 SCLK) are currently assigned to the keyboard matrix, so they
overlap SPI0 hardware functions even though LED data is connected through
GPIO10. Before simultaneous keyboard scanning and LED control are implemented,
move the keyboard connections on GPIO9 and GPIO11 to suitable unused GPIOs,
update this YAML configuration to match the physical rewiring, and do not
assume that these pins can safely be shared. GPIO14 and GPIO15 may also
conflict with UART functionality when the UART is enabled.

# Deploying Nix-built binary to Raspberry Pi

Assumptions:
- Raspberry Pi runs Debian (or similar), has **SSH** set up (ideally with passwordless keys).  
- Nix is already installed (multi-user daemon).  
- You’re building on a more powerful host and want to copy a built closure across.  

---

## 1. Build on host machine
```bash
# From your project root
nix build .#{attr-name} --no-link --print-out-paths
```

- Replace `{attr-name}` with your package (e.g. `kvmSwitch-aarch64-gnu`).  
- Output will be a store path like:

```
/nix/store/xxxx...-kvmSwitch-aarch64-unknown-linux-gnu-0.1.0.0
```

Save that as `{store-path}`.

---

## 2. Configure Nix on the Pi
SSH in:
```bash
ssh {pi-user}@{pi-ip}
```

Then configure nix-daemon:

```bash
sudo mkdir -p /etc/nix
sudo tee /etc/nix/nix.conf >/dev/null <<'EOF'
experimental-features = nix-command flakes
trusted-users = root {pi-user}
require-sigs = false
EOF

sudo systemctl restart nix-daemon || sudo systemctl start nix-daemon
```

### Important: make nix tools visible to non-login SSH sessions
This step is often missed and will break `nix copy` if omitted:

```bash
sudo ln -sf /nix/var/nix/profiles/default/bin/nix       /usr/local/bin/nix
sudo ln -sf /nix/var/nix/profiles/default/bin/nix-store /usr/local/bin/nix-store
```

---

## 3. Copy the closure across
From your host machine:
```bash
nix copy --to "ssh://{pi-user}@{pi-ip}" {store-path}
```

---

## 4. Install into profile on the Pi
SSH in again (or stay connected) and install:

```bash
nix --extra-experimental-features "nix-command flakes" \
  profile install {store-path}

# Verify it's available
command -v {binary-name}
{binary-name} --help
```

---

## 5. (Optional) Expose globally
If you want a stable `/usr/local/bin/{binary-name}`:

```bash
sudo ln -sf ~/.nix-profile/bin/{binary-name} /usr/local/bin/{binary-name}
```

---

✅ Now you can run `{binary-name}` from anywhere on the Pi.
