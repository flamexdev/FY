
set -e
cd "$(dirname "$0")"

if [ -f src/fy.nanorc ]; then
    touch "$HOME/.nanorc"
    if ! grep -Fq 'FY' "$HOME/.nanorc"; then
        cat src/fy.nanorc >> "$HOME/.nanorc"
    fi
fi

if [ -f src/fy_vm.cpp ]; then
    clang++ -std=c++17 -O2 src/fy_vm.cpp -o fy_vm
elif [ -f src/fy.cpp ]; then
    clang++ -std=c++17 -O2 src/fy.cpp -o fy_vm
fi

cat > fy <<'EOF'
#!/data/data/com.termux/files/usr/bin/bash
set -e
DIR="$(cd "$(dirname "$0")" && pwd)"

case "${1:-}" in
    --help|-h)
        echo "FY - Dual Engine"
        echo "Usage: fy [--compat|--vm] FILE.fy"
        exit 0
        ;;
    --compat)
        shift
        exec python3 "$@"
        ;;
    --vm)
        shift
        exec "$DIR/fy_vm" "$@"
        ;;
    *)
        exec python3 "$@"
        ;;
esac
EOF

chmod +x fy
chmod +x fy_vm 2>/dev/null || true

if [ -f Makefile ]; then
    make
elif [ -f CMakeLists.txt ]; then
    cmake -S . -B build
    cmake --build build
elif [ -f pyproject.toml ]; then
    python -m pip install .
elif [ -f setup.py ]; then
    python setup.py build
fi
