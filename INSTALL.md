# Installation and Usage Guide

## Prerequisites

- OCaml 5.0+ and opam
- Alpha Vantage API key ([Get one free](https://www.alphavantage.co/support/#api-key))

## Installation

### 1. Clone the Repository

```bash
git clone <repository-url>
cd FinalProject
```

### 2. Install OCaml Dependencies

**For CLI only (recommended):**
```bash
opam install cohttp-lwt-unix yojson lwt lwt_ppx lwt_ssl
```

**For GUI support (optional):**
```bash
# First install SDL2 libraries (see step 3)
# Then install OCaml GUI packages:
opam update
opam install tsdl tsdl-ttf tsdl-image bogue
```

### 3. Install SDL2 Libraries (GUI only)

**On Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libsdl2-mixer-dev \
                 libfreetype6-dev libx11-dev libxext-dev libxtst-dev \
                 cmake build-essential git pkg-config
```

**On macOS:**
```bash
brew install sdl2 sdl2_image sdl2_ttf
```

**Note:** Ubuntu 22.04 ships with SDL 2.0.20. If you encounter version errors, see "Installing SDL 2.0.22 from Source" in Troubleshooting.

### 4. Set API Key

```bash
export ALPHAVANTAGE_API_KEY=your_key_here
```

## Building

```bash
dune build
```

## Running

### Refresh Stock Cache (Recommended First)

```bash
# Refresh all default stocks (20 stocks, ~5 minutes)
dune exec -- FinalProject-refresh

# Or refresh specific stocks
dune exec -- FinalProject-refresh AAPL,MSFT,GOOGL
```

**Note:** Free Alpha Vantage API allows 5 requests/minute, so the refresh waits 15 seconds between stocks.

### Run CLI Version

```bash
dune exec -- FinalProject
```

This will:
1. Ask questions about your investment preferences
2. Calculate your risk profile
3. Analyze current investments (if any)
4. Provide personalized stock recommendations

### Run GUI Version

```bash
dune exec -- FinalProject gui
```

**Note:** Requires SDL 2.0.22+. If you're on Ubuntu 22.04 with SDL 2.0.20, use CLI instead or see troubleshooting.

## Example Workflow

```bash
# 1. Set API key
export ALPHAVANTAGE_API_KEY=your_key_here

# 2. Build project
dune build

# 3. Refresh cache with test stocks
dune exec -- FinalProject refresh AAPL,MSFT,GOOGL,NVDA

# 4. Run questionnaire
dune exec -- FinalProject
```

## Troubleshooting

### "Set the ALPHAVANTAGE_API_KEY environment variable"
Make sure you've exported your API key: `export ALPHAVANTAGE_API_KEY=your_key_here`

### "No recommendations available"
Run the refresh command first: `dune exec -- FinalProject-refresh`

### API rate limit errors
Wait a few minutes and try again, or refresh fewer stocks at once.

### GUI/SDL version errors
- **Quick fix:** Use CLI version: `dune exec -- FinalProject`
- **For GUI:** Install SDL 2.0.22 from source (see below)

### Installing SDL 2.0.22 from Source (Ubuntu 22.04)

If standard installation doesn't work:

```bash
# Install build dependencies
sudo apt update
sudo apt install build-essential cmake \
                 libasound2-dev libpulse-dev \
                 libx11-dev libxext-dev libxrandr-dev \
                 libxcursor-dev libxi-dev libxinerama-dev \
                 libxxf86vm-dev libxss-dev \
                 libgl1-mesa-dev libdbus-1-dev libudev-dev \
                 libgles2-mesa-dev libegl1-mesa-dev \
                 libibus-1.0-dev libsamplerate0-dev \
                 libwayland-dev libxkbcommon-dev \
                 libdrm-dev libgbm-dev

# Download and build SDL 2.0.22
cd /tmp
wget https://www.libsdl.org/release/SDL2-2.0.22.tar.gz
tar -xzf SDL2-2.0.22.tar.gz
cd SDL2-2.0.22
mkdir build && cd build
cmake ..
make -j$(nproc)
sudo make install

# Update library paths
echo "/usr/local/lib" | sudo tee /etc/ld.so.conf.d/sdl2.conf
sudo ldconfig

# Verify installation
sdl2-config --version  # Should output: 2.0.22

# Install SDL2 image and TTF
sudo apt install libsdl2-image-dev libsdl2-ttf-dev libsdl2-mixer-dev \
                 libfreetype6-dev libx11-dev libxext-dev libxtst-dev

# Install OCaml GUI libraries
opam update
opam install tsdl tsdl-ttf tsdl-image bogue

# Rebuild project
cd ~/3110/FinalProject
dune clean
dune build
dune exec -- FinalProject gui
```
