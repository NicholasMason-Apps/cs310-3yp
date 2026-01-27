#!/bin/sh

show_help() {
    echo "Usage: ./run.sh [options] <executable>"
    echo
    echo "Options:"
    echo "  -h, --help      Show this help message"
    echo "  -wsl            Build in WSL mode"
}

WSL_MODE=false

# Parse options
while [ "$1" != "" ]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        -wsl)
            WSL_MODE=true
            shift
            ;;
        -*)
            echo "Unknown option: $1"
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

EXEC="$1"

if [ -z "$EXEC" ]; then
    echo "Error: No executable specified"
    show_help
    exit 1
fi

# Build
if [ "$WSL_MODE" = true ]; then
    echo "Building in WSL mode..."
    stack build --flag 3yp:wsl || exit 1
else
    echo "Building in normal mode..."
    stack build || exit 1
fi

# Run
echo "Running $EXEC-exe..."
stack exec "${EXEC}-exe"