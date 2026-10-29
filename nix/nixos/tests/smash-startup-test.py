"""Exercise the startup client against an HTTP server, without a chain database."""

import base64
from contextlib import redirect_stderr
from http.server import BaseHTTPRequestHandler, HTTPServer
import importlib.util
import io
import json
from pathlib import Path
import sys
import tempfile
from threading import Thread
import unittest
from unittest.mock import patch

from bech32 import bech32_encode, convertbits

spec = importlib.util.spec_from_file_location("startup", sys.argv.pop(1))
startup = importlib.util.module_from_spec(spec)
spec.loader.exec_module(startup)
POOL = "ab" * 28
MANUAL = "cd" * 28
PASSWORD = 'a,comma:colon"quote\\slash'
AUTH = "Basic " + base64.b64encode(f"admin:{PASSWORD}".encode()).decode()


class StartupTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.admins = Path(self.temp.name) / "admins"
        self.admins.write_text(f"\n admin , {PASSWORD} \nother,unused\n")
        self.delisted = {MANUAL}
        self.writes = []
        self.failure = None
        self.persist = True
        self.ready = True
        self.bad_list = False
        test = self

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *args):
                pass

            def reply(self, status, body):
                self.send_response(status)
                self.end_headers()
                self.wfile.write(json.dumps(body).encode())

            def do_GET(self):
                # Public reads must never receive administrator credentials.
                test.assertIsNone(self.headers.get("Authorization"))
                if self.path == "/api/v1/status":
                    self.reply(200 if test.ready else 503, {"status": "OK"})
                elif self.path == "/api/v1/delisted":
                    self.reply(200, {} if test.bad_list else
                               [{"poolId": p} for p in sorted(test.delisted)])
                else:
                    self.reply(404, {})

            def do_PATCH(self):
                body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
                test.writes.append(body)
                if self.headers.get("Authorization") != AUTH:
                    self.reply(401, {})
                elif test.failure:
                    self.reply(test.failure, {})
                elif body["poolId"] in test.delisted:
                    self.reply(400, {"error": "already delisted"})
                else:
                    if test.persist:
                        test.delisted.add(body["poolId"])
                    self.reply(200, body)

        self.server = HTTPServer(("127.0.0.1", 0), Handler)
        self.port = self.server.server_port
        self.thread = Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.addCleanup(self.close_server)

    def close_server(self):
        self.server.shutdown()
        self.thread.join()
        self.server.server_close()

    def test_authenticated_additive_and_idempotent(self):
        startup.wait_ready(self.port)
        bech32_pool = bech32_encode("pool", convertbits(bytes.fromhex(POOL), 8, 5))
        startup.apply_delists(self.port, [POOL.upper(), bech32_pool, POOL], self.admins)
        startup.apply_delists(self.port, [bech32_pool], self.admins)
        self.assertEqual(self.delisted, {POOL, MANUAL})
        self.assertEqual(self.writes, [{"poolId": POOL}])

    def test_public_only_needs_no_admins(self):
        startup.wait_ready(self.port)
        startup.apply_delists(self.port, [], None)
        self.assertEqual(self.writes, [])

    def test_wrong_credentials_fail(self):
        self.admins.write_text("admin,wrong\n")
        with self.assertRaisesRegex(startup.StartupError, "HTTP 401"):
            startup.apply_delists(self.port, [POOL], self.admins)

    def test_server_error_fails(self):
        self.failure = 500
        with self.assertRaisesRegex(startup.StartupError, "HTTP 500"):
            startup.apply_delists(self.port, [POOL], self.admins)

    def test_redirect_is_not_followed(self):
        self.failure = 302
        with self.assertRaisesRegex(startup.StartupError, "HTTP 302"):
            startup.apply_delists(self.port, [POOL], self.admins)

    def test_verifies_persistence(self):
        self.persist = False
        with self.assertRaisesRegex(startup.StartupError, "did not retain"):
            startup.apply_delists(self.port, [POOL], self.admins)

    def test_invalid_list_response_fails(self):
        self.bad_list = True
        with self.assertRaisesRegex(startup.StartupError, "Invalid SMASH"):
            startup.apply_delists(self.port, [POOL], self.admins)

    def test_readiness_failure_is_bounded(self):
        self.ready = False
        with patch.object(startup.time, "sleep") as sleep:
            with self.assertRaisesRegex(startup.StartupError, "HTTP 503"):
                startup.wait_ready(self.port)
            self.assertEqual(sleep.call_count, 9)

    def test_invalid_admin_files(self):
        for contents in ("", "\n", "missing-comma", ",password", "admin,", "bad:name,password"):
            with self.subTest(contents=contents):
                self.admins.write_text(contents)
                with self.assertRaises(startup.StartupError):
                    startup.authorization(self.admins)

    def test_invalid_pool_id(self):
        with self.assertRaisesRegex(startup.StartupError, "Invalid pool ID"):
            startup.apply_delists(self.port, ["not-a-pool"], self.admins)
        self.assertEqual(self.writes, [])

    def test_cli_failure_does_not_print_credentials(self):
        self.failure = 401
        pools = Path(self.temp.name) / "pools.json"
        pools.write_text(json.dumps([POOL]))
        args = ["smash-startup", "--port", str(self.port), "--pools", str(pools),
                "--admins", str(self.admins)]
        output = io.StringIO()
        with patch.object(sys, "argv", args), redirect_stderr(output):
            self.assertEqual(startup.main(), 1)
        self.assertIn("HTTP 401", output.getvalue())
        self.assertNotIn(PASSWORD, output.getvalue())
        self.assertNotIn(AUTH, output.getvalue())


if __name__ == "__main__":
    unittest.main()
