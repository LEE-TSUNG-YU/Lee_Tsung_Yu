let appData;
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
