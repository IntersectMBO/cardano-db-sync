"""Wait for SMASH and apply additive, authenticated startup delisting."""

import argparse
import base64
import http.client
import json
from pathlib import Path
import re
import sys
import time

from bech32 import bech32_decode, convertbits


class StartupError(Exception):
    pass


def pool_id(value):
    if isinstance(value, str):
        if re.fullmatch(r"[0-9a-fA-F]{56}", value):
            return value.lower()
        prefix, data = bech32_decode(value)
        if prefix == "pool" and data is not None:
            decoded = convertbits(data, 5, 8, False)
            if decoded is not None and len(decoded) == 28:
                return bytes(decoded).hex()
    raise StartupError("Invalid pool ID in configuration or SMASH response")


def authorization(path):
    # Match SMASH's first-comma split and whitespace trimming, not CSV quoting.
    for line in Path(path).read_text(encoding="utf-8").splitlines():
        if not line:
            continue
        user, comma, password = line.partition(",")
        user, password = user.strip(), password.strip()
        if not comma or not user or not password or ":" in user:
            raise StartupError("First SMASH administrator entry is invalid for Basic Auth")
        token = base64.b64encode(f"{user}:{password}".encode()).decode("ascii")
        return f"Basic {token}"
    raise StartupError("SMASH administrator file is empty")


def request(port, method, path, body=None, auth=None, timeout=10):
    # Direct loopback connection: no environment proxies or redirect following.
    connection = http.client.HTTPConnection("127.0.0.1", port, timeout=timeout)
    headers = {"Content-Type": "application/json"}
    if auth is not None:
        headers["Authorization"] = auth
    try:
        connection.request(method, path, body=json.dumps(body) if body is not None else None,
                           headers=headers)
        response = connection.getresponse()
        if response.status != 200:
            # Do not log headers, credentials, or a potentially reflecting body.
            raise StartupError(f"SMASH {method} {path} returned HTTP {response.status}")
        return json.loads(response.read())
    except (OSError, http.client.HTTPException, ValueError) as error:
        raise StartupError(f"SMASH {method} {path} failed ({type(error).__name__})") from None
    finally:
        connection.close()


def wait_ready(port):
    for attempt in range(10):
        try:
            request(port, "GET", "/api/v1/status", timeout=2)
            return
        except StartupError:
            if attempt == 9:
                raise
            time.sleep(2)


def get_delisted(port):
    response = request(port, "GET", "/api/v1/delisted")
    if not isinstance(response, list):
        raise StartupError("Invalid SMASH delisted-pools response")
    try:
        return {pool_id(entry["poolId"]) for entry in response}
    except (KeyError, TypeError):
        raise StartupError("Invalid SMASH delisted-pools response") from None


def apply_delists(port, pools, admins):
    wanted = {pool_id(value) for value in pools}
    if not wanted:
        return
    if admins is None:
        raise StartupError("An administrator file is required for startup delisting")
    auth = authorization(admins)
    existing = get_delisted(port)
    for pool in sorted(wanted - existing):
        print(f"Delisting pool {pool}.", flush=True)
        result = request(port, "PATCH", "/api/v1/delist", {"poolId": pool}, auth)
        if not isinstance(result, dict) or result.get("poolId") != pool:
            raise StartupError("SMASH did not confirm the requested delist")
    if not wanted <= get_delisted(port):
        raise StartupError("SMASH did not retain all configured delists")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, required=True)
    parser.add_argument("--pools", type=Path, required=True)
    parser.add_argument("--admins", type=Path)
    args = parser.parse_args()
    try:
        pools = json.loads(args.pools.read_text())
        wait_ready(args.port)
        apply_delists(args.port, pools, args.admins)
    except (StartupError, OSError, ValueError) as error:
        # File/JSON errors are summarized too, without printing secret contents.
        message = str(error) if isinstance(error, StartupError) else type(error).__name__
        print(f"SMASH startup failed: {message}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
