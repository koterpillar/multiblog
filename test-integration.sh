#!/bin/bash
set -euo pipefail

# Integration test: start the binary with test content, hit the home page, check content.

BINARY=""
DOCKER_IMAGE=""

while [ "$#" -gt 0 ]; do
    case "$1" in
        --binary)
            if [ "$#" -lt 2 ]; then
                echo "FAIL: --binary requires a path" >&2
                exit 2
            fi
            if [ -n "$DOCKER_IMAGE" ]; then
                echo "FAIL: --binary and --docker are mutually exclusive" >&2
                exit 2
            fi
            BINARY="$2"
            shift 2
            ;;
        --docker)
            if [ "$#" -lt 2 ]; then
                echo "FAIL: --docker requires an image name" >&2
                exit 2
            fi
            if [ -n "$BINARY" ]; then
                echo "FAIL: --binary and --docker are mutually exclusive" >&2
                exit 2
            fi
            DOCKER_IMAGE="$2"
            shift 2
            ;;
        *)
            echo "FAIL: unknown argument: $1" >&2
            exit 2
            ;;
    esac
done

if [ -z "$BINARY" ] && [ -z "$DOCKER_IMAGE" ]; then
    BINARY="$(stack path --local-install-root)/bin/multiblog"
fi

CONTENT_DIR="$(cd testsuite/Integration/content && pwd)"
PORT=18742

export PORT
export CONTENT_DIRECTORY="$CONTENT_DIR"
export SITE_URL="http://localhost:$PORT"

PDF_HEADERS=""
PDF_BODY=""

# Start the server in the background
if [ -n "$DOCKER_IMAGE" ]; then
    CONTAINER_NAME="multiblog-it-${PORT}-$$"
    docker run -d --rm \
        --name "$CONTAINER_NAME" \
        -p "$PORT:$PORT" \
        -e PORT="$PORT" \
        -e SITE_URL="$SITE_URL" \
        -v "$CONTENT_DIR:/content:ro" \
        "$DOCKER_IMAGE" >/dev/null
    SERVER_PID=""
else
    "$BINARY" &
    SERVER_PID=$!
fi

cleanup() {
    if [ -n "$DOCKER_IMAGE" ]; then
        docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true
    else
        kill "$SERVER_PID" 2>/dev/null || true
    fi
    if [ -n "$PDF_HEADERS" ] || [ -n "$PDF_BODY" ]; then
        rm -f "$PDF_HEADERS" "$PDF_BODY" 2>/dev/null || true
    fi
}
trap cleanup EXIT

# Wait for the server to be ready (up to 10 seconds)
i=0
until curl -sf "http://localhost:$PORT/" >/dev/null 2>&1; do
    i=$((i + 1))
    if [ "$i" -ge 20 ]; then
        echo "FAIL: server did not start in time"
        exit 1
    fi
    sleep 0.5
done

FAIL=0

check_contains() {
    label="$1"
    expected="$2"
    body="$3"
    if echo "$body" | grep -qF "$expected"; then
        echo "PASS: $label"
    else
        echo "FAIL: $label (expected: $expected)"
        FAIL=1
    fi
}

check_not_contains() {
    label="$1"
    unexpected="$2"
    body="$3"
    if echo "$body" | grep -qF "$unexpected"; then
        echo "FAIL: $label (should not contain: $unexpected)"
        FAIL=1
    else
        echo "PASS: $label"
    fi
}

BODY=$(curl -sf "http://localhost:$PORT/")

check_contains     "site name present"            "Test site"              "$BODY"
check_contains     "first test article listed"    "First test article"     "$BODY"
check_contains     "another article listed"       "Another article"        "$BODY"
check_contains     "next page link present"       "Next page"              "$BODY"
check_not_contains "previous page absent on p1"   "Previous page"          "$BODY"
check_not_contains "early article not on page 1"  "Very early article"     "$BODY"

PDF_HEADERS="$(mktemp)"
PDF_BODY="$(mktemp)"
if curl -sf -D "$PDF_HEADERS" -o "$PDF_BODY" "http://localhost:$PORT/meta.pdf" >/dev/null 2>&1; then
    if head -c 4 "$PDF_BODY" | grep -q '^%PDF$'; then
        echo "PASS: pdf magic bytes"
    else
        echo "FAIL: pdf magic bytes (expected: %PDF)"
        FAIL=1
    fi
else
    echo "FAIL: pdf endpoint request"
    FAIL=1
fi

if grep -qi '^Content-Type: application/pdf' "$PDF_HEADERS"; then
    echo "PASS: pdf content-type"
else
    echo "FAIL: pdf content-type (expected: application/pdf)"
    FAIL=1
fi

if grep -qi '^Content-Disposition: inline; filename="meta.pdf"' "$PDF_HEADERS"; then
    echo "PASS: pdf content-disposition"
else
    echo "FAIL: pdf content-disposition (expected inline meta.pdf)"
    FAIL=1
fi

exit $FAIL
