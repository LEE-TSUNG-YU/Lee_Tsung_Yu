const API_KEY = "d87vqspr01qmhakhj7j0d87vqspr01qmhakhj7jg";

async function getStock() {
  const symbol = document.getElementById("symbol").value;

  if (!symbol) return;

  const quoteRes = await fetch(
    `https://finnhub.io/api/v1/quote?symbol=${symbol}&token=${API_KEY}`
  );
  const quote = await quoteRes.json();

  document.getElementById("price").innerHTML = `
    <h2>${symbol.toUpperCase()}</h2>
    <p>Current Price: $${quote.c}</p>
    <p>High: ${quote.h} | Low: ${quote.l}</p>
  `;

  const to = Math.floor(Date.now() / 1000);
  const from = to - 60 * 60 * 24 * 365;

  const candleRes = await fetch(
    `https://finnhub.io/api/v1/stock/candle?symbol=${symbol}&resolution=D&from=${from}&to=${to}&token=${API_KEY}`
  );

  const candle = await candleRes.json();

  const trace = {
    x: candle.t.map(t => new Date(t * 1000)),
    open: candle.o,
    high: candle.h,
    low: candle.l,
    close: candle.c,
    type: "candlestick"
  };

  Plotly.newPlot("chart", [trace], {
    title: `${symbol.toUpperCase()} Price Chart`
  });
}
