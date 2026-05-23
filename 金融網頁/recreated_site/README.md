# 財經期末報告成果重現

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
