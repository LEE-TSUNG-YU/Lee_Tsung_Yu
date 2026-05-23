from __future__ import annotations

from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path


ROOT = Path(__file__).resolve().parent
SITE_DIR = ROOT / "recreated_site"
HOST = "127.0.0.1"
PORT = 8765


def main() -> None:
    if not (SITE_DIR / "index.html").exists():
        raise SystemExit("找不到 recreated_site/index.html，請先執行 python recreate_finance_site.py")

    handler = partial(SimpleHTTPRequestHandler, directory=str(SITE_DIR))
    server = ThreadingHTTPServer((HOST, PORT), handler)
    print(f"財經期末成果重現網站已啟動：http://{HOST}:{PORT}/index.html")
    print("按 Ctrl+C 可停止伺服器。")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\n伺服器已停止。")
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
