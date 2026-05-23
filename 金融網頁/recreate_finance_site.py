from __future__ import annotations

import json
import math
import random
from dataclasses import dataclass
from datetime import date, timedelta
from pathlib import Path


ROOT = Path(__file__).resolve().parent
OUT = ROOT / "recreated_site"


@dataclass(frozen=True)
class StockProfile:
    symbol: str
    name: str
    base_price: float
    drift: float
    volatility: float


STOCKS = [
    StockProfile("2330", "台積電", 620.0, 0.0008, 0.018),
    StockProfile("2317", "鴻海", 142.0, 0.0004, 0.014),
    StockProfile("2454", "聯發科", 980.0, 0.0006, 0.020),
    StockProfile("2881", "富邦金", 72.0, 0.0002, 0.010),
    StockProfile("2303", "聯電", 52.0, 0.0003, 0.017),
]


def trading_days(start: date, end: date) -> list[date]:
    days: list[date] = []
    cursor = start
    while cursor <= end:
        if cursor.weekday() < 5:
            days.append(cursor)
        cursor += timedelta(days=1)
    return days


def generate_ohlcv(profile: StockProfile, days: list[date], seed: int) -> list[dict]:
    rng = random.Random(seed)
    close = profile.base_price
    rows: list[dict] = []

    for day in days:
        seasonal = math.sin(day.toordinal() / 17) * profile.volatility * 0.45
        shock = rng.gauss(profile.drift + seasonal, profile.volatility)
        open_price = close * (1 + rng.gauss(0, profile.volatility / 3))
        close = max(10, open_price * (1 + shock))
        high = max(open_price, close) * (1 + abs(rng.gauss(0, profile.volatility / 2)))
        low = min(open_price, close) * (1 - abs(rng.gauss(0, profile.volatility / 2)))
        volume = int(rng.uniform(18000, 82000) * (1 + abs(shock) * 12))

        rows.append(
            {
                "date": day.isoformat(),
                "open": round(open_price, 2),
                "high": round(high, 2),
                "low": round(low, 2),
                "close": round(close, 2),
                "volume": volume,
            }
        )

    return rows


def low_buy_high_sell(rows: list[dict]) -> dict:
    trades: list[dict] = []
    holding: dict | None = None

    for index, row in enumerate(rows):
        body = abs(row["close"] - row["open"])
        lower_shadow = min(row["open"], row["close"]) - row["low"]
        is_red = row["close"] > row["open"]

        if holding is None and is_red and lower_shadow >= body * 1.8:
            holding = {
                "entry_date": row["date"],
                "entry_price": row["close"],
                "entry_index": index,
            }
            continue

        if holding and index - holding["entry_index"] >= 3 and is_red:
            ret = (row["close"] - holding["entry_price"]) / holding["entry_price"]
            trades.append(
                {
                    "entry_date": holding["entry_date"],
                    "exit_date": row["date"],
                    "entry_price": round(holding["entry_price"], 2),
                    "exit_price": round(row["close"], 2),
                    "return_pct": round(ret * 100, 2),
                }
            )
            holding = None

    total_return = round(sum(t["return_pct"] for t in trades), 2)
    win_rate = round(
        sum(1 for t in trades if t["return_pct"] > 0) / len(trades) * 100, 2
    ) if trades else 0
    return {"trades": trades[-8:], "total_return_pct": total_return, "win_rate": win_rate}


def moving_average(values: list[float], window: int) -> list[float | None]:
    output: list[float | None] = []
    for index in range(len(values)):
        if index + 1 < window:
            output.append(None)
        else:
            output.append(round(sum(values[index + 1 - window:index + 1]) / window, 2))
    return output


def detect_signals(rows: list[dict]) -> list[dict]:
    closes = [row["close"] for row in rows]
    ma20 = moving_average(closes, 20)
    ma60 = moving_average(closes, 60)
    signals: list[dict] = []

    for index in range(1, len(rows)):
        if not ma20[index] or not ma60[index] or not ma20[index - 1] or not ma60[index - 1]:
            continue

        row = rows[index]
        yesterday = rows[index - 1]
        signal_names: list[str] = []
        direction = "Neutral"

        if row["open"] > yesterday["high"] * 1.01:
            signal_names.append("up_gap")
        if row["open"] < yesterday["low"] * 0.99:
            signal_names.append("down_gap")
        if ma20[index] > ma60[index] and ma20[index - 1] <= ma60[index - 1]:
            signal_names.append("support")
            direction = "Long"
        if ma20[index] < ma60[index] and ma20[index - 1] >= ma60[index - 1]:
            signal_names.append("resistance")
            direction = "Short"
        if row["close"] > row["open"] and row["volume"] > yesterday["volume"] * 1.25:
            signal_names.append("bar")
            direction = "Long"

        if len(signal_names) >= 2:
            signals.append(
                {
                    "date": row["date"],
                    "direction": direction,
                    "count": len(signal_names),
                    "signals": ", ".join(signal_names),
                    "close": row["close"],
                }
            )

    return signals[-10:]


def broker_report_strategy(price_map: dict[str, list[dict]]) -> list[dict]:
    rng = random.Random(202601)
    rows: list[dict] = []
    for symbol, prices in price_map.items():
        for idx in range(80, len(prices), 29):
            row = prices[idx]
            entry = prices[min(idx + 1, len(prices) - 1)]
            exit_row = prices[min(idx + rng.randint(12, 34), len(prices) - 1)]
            ret = (exit_row["open"] - entry["open"]) / entry["open"] * 100
            buy_reports = rng.randint(5, 16)
            if buy_reports < 7:
                continue
            rows.append(
                {
                    "symbol": symbol,
                    "report_date": row["date"],
                    "buy_reports": buy_reports,
                    "entry_date": entry["date"],
                    "entry_price": entry["open"],
                    "exit_date": exit_row["date"],
                    "exit_price": exit_row["open"],
                    "return_pct": round(ret, 2),
                    "result": "Win" if ret > 0 else "Loss",
                }
            )
    rows.sort(key=lambda item: (item["report_date"], -item["buy_reports"]))
    return rows[-16:]


def build_data() -> dict:
    days = trading_days(date(2025, 1, 2), date(2025, 12, 31))
    price_map = {
        profile.symbol: generate_ohlcv(profile, days, seed=1000 + index)
        for index, profile in enumerate(STOCKS)
    }
    stocks = []
    for profile in STOCKS:
        rows = price_map[profile.symbol]
        last = rows[-1]
        previous = rows[-2]
        stocks.append(
            {
                "symbol": profile.symbol,
                "name": profile.name,
                "latest": last["close"],
                "change_pct": round((last["close"] - previous["close"]) / previous["close"] * 100, 2),
                "prices": rows,
                "ma20": moving_average([row["close"] for row in rows], 20),
                "ma60": moving_average([row["close"] for row in rows], 60),
                "strategy": low_buy_high_sell(rows),
                "signals": detect_signals(rows),
            }
        )

    return {
        "generated_at": date.today().isoformat(),
        "stocks": stocks,
        "report_backtest": broker_report_strategy(price_map),
    }


def write_file(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    data = build_data()
    write_file(OUT / "data.json", json.dumps(data, ensure_ascii=False, indent=2))
    write_file(OUT / "index.html", HTML)
    write_file(OUT / "style.css", CSS)
    write_file(OUT / "app.js", JS)
    write_file(OUT / "README.md", README)
    write_file(ROOT / "run_recreated_site.py", RUNNER)
    print(f"重現版金融分析網站已建立：{OUT}")
    print("啟動網站：python run_recreated_site.py")
    print("瀏覽網址：http://127.0.0.1:8765/index.html")


HTML = """<!DOCTYPE html>
<html lang="zh-Hant">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>財經期末報告成果重現</title>
  <script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
  <link rel="stylesheet" href="style.css">
</head>
<body>
  <aside class="sidebar">
    <div class="brand">
      <span class="brand-mark">LT</span>
      <div>
        <p>財經分析工具</p>
        <strong>期末成果重現</strong>
      </div>
    </div>
    <nav>
      <button class="active" data-view="dashboard">股票分析</button>
      <button data-view="strategy">策略回測</button>
      <button data-view="signals">Long／Short 訊號</button>
      <button data-view="reports">券商報告策略</button>
    </nav>
  </aside>

  <main>
    <header class="topbar">
      <div>
        <h1>台股金融分析 Dashboard</h1>
        <p>以 Python 產生模擬 OHLCV、策略結果與訊號資料，重現範本中的核心展示流程。</p>
      </div>
      <label>
        股票
        <select id="stockSelect"></select>
      </label>
    </header>

    <section id="dashboard" class="view active">
      <div class="metrics">
        <article>
          <span>最新收盤</span>
          <strong id="latestPrice"></strong>
        </article>
        <article>
          <span>日漲跌幅</span>
          <strong id="changePct"></strong>
        </article>
        <article>
          <span>策略勝率</span>
          <strong id="winRate"></strong>
        </article>
      </div>
      <div id="priceChart" class="chart"></div>
    </section>

    <section id="strategy" class="view">
      <div class="section-head">
        <h2>低買高賣策略</h2>
        <p>條件：收紅 K 且下影線接近實體 K 的兩倍時進場，至少持有三個交易日後以紅 K 出場。</p>
      </div>
      <div class="summary-card" id="strategySummary"></div>
      <div class="table-wrap">
        <table id="tradeTable"></table>
      </div>
    </section>

    <section id="signals" class="view">
      <div class="section-head">
        <h2>Long／Short 訊號偵測</h2>
        <p>整合 up gap、down gap、bar、support、resistance 等條件，列出多訊號交集。</p>
      </div>
      <div class="table-wrap">
        <table id="signalTable"></table>
      </div>
    </section>

    <section id="reports" class="view">
      <div class="section-head">
        <h2>券商報告策略</h2>
        <p>模擬 30 日內熱門買進評價股票，依隔日開盤價進場並以後續開盤價出場。</p>
      </div>
      <div id="reportChart" class="chart small"></div>
      <div class="table-wrap">
        <table id="reportTable"></table>
      </div>
    </section>
  </main>

  <script src="app.js"></script>
</body>
</html>
"""


CSS = """* {
  box-sizing: border-box;
}

body {
  margin: 0;
  min-height: 100vh;
  display: grid;
  grid-template-columns: 260px 1fr;
  background: #f5f7fb;
  color: #172033;
  font-family: Arial, "Noto Sans TC", sans-serif;
}

.sidebar {
  background: #111827;
  color: #ffffff;
  padding: 24px 18px;
}

.brand {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 32px;
}

.brand-mark {
  display: grid;
  place-items: center;
  width: 42px;
  height: 42px;
  border-radius: 8px;
  background: #2dd4bf;
  color: #062822;
  font-weight: 800;
}

.brand p {
  margin: 0 0 4px;
  color: #a7b0c0;
  font-size: 13px;
}

.brand strong {
  font-size: 18px;
}

nav {
  display: grid;
  gap: 8px;
}

nav button {
  width: 100%;
  padding: 12px 14px;
  border: 0;
  border-radius: 6px;
  background: transparent;
  color: #cbd5e1;
  text-align: left;
  cursor: pointer;
  font-size: 15px;
}

nav button.active,
nav button:hover {
  background: #243244;
  color: #ffffff;
}

main {
  padding: 28px;
  overflow-x: hidden;
}

.topbar {
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 24px;
  margin-bottom: 24px;
}

h1,
h2,
p {
  margin-top: 0;
}

h1 {
  margin-bottom: 8px;
  font-size: 30px;
}

.topbar p,
.section-head p {
  color: #657187;
  line-height: 1.6;
}

label {
  display: grid;
  gap: 8px;
  color: #536075;
  font-weight: 700;
}

select {
  min-width: 180px;
  padding: 10px 12px;
  border: 1px solid #cfd7e6;
  border-radius: 6px;
  background: #ffffff;
  font-size: 15px;
}

.view {
  display: none;
}

.view.active {
  display: block;
}

.metrics {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 16px;
  margin-bottom: 18px;
}

.metrics article,
.summary-card,
.chart,
.table-wrap {
  border: 1px solid #e1e7f0;
  border-radius: 8px;
  background: #ffffff;
  box-shadow: 0 10px 24px rgba(15, 23, 42, 0.05);
}

.metrics article {
  padding: 18px;
}

.metrics span {
  display: block;
  margin-bottom: 10px;
  color: #657187;
  font-size: 14px;
}

.metrics strong {
  font-size: 28px;
}

.chart {
  width: 100%;
  height: 560px;
  padding: 10px;
}

.chart.small {
  height: 360px;
  margin-bottom: 18px;
}

.section-head {
  max-width: 860px;
  margin-bottom: 18px;
}

.summary-card {
  display: flex;
  gap: 28px;
  margin-bottom: 18px;
  padding: 18px;
}

.summary-card span {
  color: #657187;
}

.summary-card strong {
  display: block;
  margin-top: 6px;
  font-size: 22px;
}

.table-wrap {
  overflow-x: auto;
}

table {
  width: 100%;
  border-collapse: collapse;
}

th,
td {
  padding: 12px 14px;
  border-bottom: 1px solid #e8edf5;
  text-align: left;
  white-space: nowrap;
}

th {
  background: #f9fbfe;
  color: #42506a;
  font-size: 13px;
}

.positive {
  color: #047857;
}

.negative {
  color: #be123c;
}

@media (max-width: 860px) {
  body {
    grid-template-columns: 1fr;
  }

  .sidebar {
    position: static;
  }

  .topbar,
  .summary-card {
    display: grid;
  }

  .metrics {
    grid-template-columns: 1fr;
  }
}
"""


JS = """let appData;
let currentStock;

const currency = new Intl.NumberFormat("zh-TW", {
  minimumFractionDigits: 2,
  maximumFractionDigits: 2
});

fetch("data.json")
  .then(response => response.json())
  .then(data => {
    appData = data;
    setupNavigation();
    setupStocks();
    renderAll();
  });

function setupNavigation() {
  document.querySelectorAll("nav button").forEach(button => {
    button.addEventListener("click", () => {
      document.querySelectorAll("nav button").forEach(item => item.classList.remove("active"));
      document.querySelectorAll(".view").forEach(item => item.classList.remove("active"));
      button.classList.add("active");
      document.getElementById(button.dataset.view).classList.add("active");
      if (button.dataset.view === "reports") renderReportBacktest();
    });
  });
}

function setupStocks() {
  const select = document.getElementById("stockSelect");
  select.innerHTML = appData.stocks
    .map(stock => `<option value="${stock.symbol}">${stock.symbol}　${stock.name}</option>`)
    .join("");
  select.addEventListener("change", renderAll);
}

function getSelectedStock() {
  const symbol = document.getElementById("stockSelect").value || appData.stocks[0].symbol;
  return appData.stocks.find(stock => stock.symbol === symbol);
}

function renderAll() {
  currentStock = getSelectedStock();
  renderMetrics();
  renderPriceChart();
  renderStrategy();
  renderSignals();
  renderReportBacktest();
}

function renderMetrics() {
  document.getElementById("latestPrice").textContent = currency.format(currentStock.latest);
  const changeNode = document.getElementById("changePct");
  changeNode.textContent = `${currentStock.change_pct}%`;
  changeNode.className = currentStock.change_pct >= 0 ? "positive" : "negative";
  document.getElementById("winRate").textContent = `${currentStock.strategy.win_rate}%`;
}

function renderPriceChart() {
  const prices = currentStock.prices;
  const x = prices.map(row => row.date);

  Plotly.newPlot("priceChart", [
    {
      x,
      open: prices.map(row => row.open),
      high: prices.map(row => row.high),
      low: prices.map(row => row.low),
      close: prices.map(row => row.close),
      type: "candlestick",
      name: `${currentStock.symbol} K 線`,
      increasing: { line: { color: "#ef4444" } },
      decreasing: { line: { color: "#0f766e" } }
    },
    {
      x,
      y: currentStock.ma20,
      type: "scatter",
      mode: "lines",
      name: "MA20",
      line: { color: "#2563eb", width: 1.6 }
    },
    {
      x,
      y: currentStock.ma60,
      type: "scatter",
      mode: "lines",
      name: "MA60",
      line: { color: "#f59e0b", width: 1.6 }
    }
  ], chartLayout(`${currentStock.symbol} ${currentStock.name} 股價走勢`), { responsive: true });
}

function renderStrategy() {
  const strategy = currentStock.strategy;
  document.getElementById("strategySummary").innerHTML = `
    <div><span>總報酬率</span><strong class="${strategy.total_return_pct >= 0 ? "positive" : "negative"}">${strategy.total_return_pct}%</strong></div>
    <div><span>勝率</span><strong>${strategy.win_rate}%</strong></div>
    <div><span>近期交易筆數</span><strong>${strategy.trades.length}</strong></div>
  `;
  renderTable("tradeTable", ["進場日", "出場日", "進場價", "出場價", "報酬率"], strategy.trades.map(row => [
    row.entry_date,
    row.exit_date,
    currency.format(row.entry_price),
    currency.format(row.exit_price),
    formatPercent(row.return_pct)
  ]));
}

function renderSignals() {
  renderTable("signalTable", ["日期", "方向", "訊號數", "訊號內容", "收盤價"], currentStock.signals.map(row => [
    row.date,
    row.direction,
    row.count,
    row.signals,
    currency.format(row.close)
  ]));
}

function renderReportBacktest() {
  const rows = appData.report_backtest;
  renderTable("reportTable", ["股票", "報告日", "買進評價數", "進場日", "出場日", "出場價", "報酬率", "結果"], rows.map(row => [
    row.symbol,
    row.report_date,
    row.buy_reports,
    row.entry_date,
    row.exit_date,
    currency.format(row.exit_price),
    formatPercent(row.return_pct),
    row.result
  ]));

  Plotly.newPlot("reportChart", [{
    x: rows.map(row => `${row.symbol} ${row.entry_date}`),
    y: rows.map(row => row.return_pct),
    type: "bar",
    marker: { color: rows.map(row => row.return_pct >= 0 ? "#0f766e" : "#be123c") },
    name: "報酬率"
  }], chartLayout("券商報告策略回測報酬率"), { responsive: true });
}

function renderTable(id, headers, rows) {
  const empty = `<tr><td colspan="${headers.length}">目前沒有符合條件的資料。</td></tr>`;
  document.getElementById(id).innerHTML = `
    <thead><tr>${headers.map(header => `<th>${header}</th>`).join("")}</tr></thead>
    <tbody>${rows.length ? rows.map(row => `<tr>${row.map(cell => `<td>${cell}</td>`).join("")}</tr>`).join("") : empty}</tbody>
  `;
}

function formatPercent(value) {
  const className = value >= 0 ? "positive" : "negative";
  return `<span class="${className}">${value}%</span>`;
}

function chartLayout(title) {
  return {
    title,
    margin: { l: 56, r: 24, t: 48, b: 48 },
    paper_bgcolor: "#ffffff",
    plot_bgcolor: "#ffffff",
    font: { family: "Arial, Noto Sans TC, sans-serif", color: "#172033" },
    xaxis: { rangeslider: { visible: false }, gridcolor: "#eef2f7" },
    yaxis: { gridcolor: "#eef2f7" },
    legend: { orientation: "h", y: 1.08 }
  };
}
"""


README = """# 財經期末報告成果重現

此資料夾由 `recreate_finance_site.py` 產生，重現「財經期末報告範本  廖子齊」中的核心成果：

- 股票代號選擇與 K 線圖
- MA20／MA60 技術線
- 低買高賣策略回測
- Long／Short 多訊號交集
- 券商報告熱門買進策略回測

## 使用方式

1. 執行 `python recreate_finance_site.py`
2. 執行 `python run_recreated_site.py`
3. 開啟 `http://127.0.0.1:8765/index.html`

資料為 Python 產生的離線模擬資料，因此不需要 API key、資料庫或 Django 伺服器。
"""


RUNNER = """from __future__ import annotations

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
        print("\\n伺服器已停止。")
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
"""


if __name__ == "__main__":
    main()
