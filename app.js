
async function loadStock(){

    const symbol =
        document.getElementById("stockInput")
        .value
        .toUpperCase()
        .trim();

    if(!symbol){

        alert("請輸入股票代碼");

        return;
    }

    document.getElementById("status")
        .innerHTML =
        "讀取資料中...";

    try{

        // 使用 Stooq 免費資料源
        // 不需要 API Key

        const url =
            `https://stooq.com/q/d/l/?s=${symbol.toLowerCase()}&i=d`;

        const response =
            await fetch(url);

        const csv =
            await response.text();

        if(csv.includes("No data")){

            throw new Error("找不到股票資料");
        }

        const rows =
            csv.trim().split("\n");

        rows.shift();

        const dates = [];
        const closes = [];

        rows.forEach(row => {

            const cols = row.split(",");

            if(cols.length >= 5){

                dates.push(cols[0]);
                closes.push(Number(cols[4]));
            }

        });

        if(closes.length === 0){

            throw new Error("沒有可用資料");
        }

        const latest =
            closes[closes.length - 1];

        const change =
            latest - closes[closes.length - 2];

        document.getElementById("status")
            .innerHTML = `
            <h2>${symbol}</h2>
            <p>最新收盤價：${latest.toFixed(2)}</p>
            <p>漲跌：${change.toFixed(2)}</p>
        `;

        const data = [{

            x:dates,
            y:closes,
            type:"scatter",
            mode:"lines",
            name:symbol

        }];

        const layout = {

            title:`${symbol} 股價走勢圖`,
            xaxis:{
                title:"日期"
            },
            yaxis:{
                title:"價格"
            }

        };

        Plotly.newPlot(
            "chart",
            data,
            layout,
            {
                responsive:true
            }
        );

    }
    catch(error){

        document.getElementById("status")
            .innerHTML =
            "載入失敗，請確認股票代碼。";

        console.error(error);
    }

}
