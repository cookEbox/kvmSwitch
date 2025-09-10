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
