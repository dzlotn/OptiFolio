# OptiFolio 📈

> A personalized portfolio optimization tool built in OCaml that analyzes your investment preferences and recommends stocks tailored to your risk profile.

OptiFolio is an intelligent portfolio optimization system that helps investors build personalized stock portfolios based on their risk tolerance, investment goals, and financial preferences. By combining user questionnaires with real-time stock data analysis, OptiFolio provides data-driven investment recommendations.

## ✨ Features

- **📋 Interactive Questionnaire**: Comprehensive 9-question assessment covering investment goals, experience, risk tolerance, and time horizon
- **🎯 Risk Profiling**: Converts qualitative responses into quantitative risk metrics (volatility, Sharpe ratio, max drawdown tolerance)
- **📊 Financial Analytics**: Advanced calculations including annualized volatility, Sharpe ratio, maximum drawdown, and cumulative returns
- **🔄 Real-time Stock Data**: Integration with Alpha Vantage API for up-to-date market data
- **💾 Smart Caching**: Local cache system to minimize API calls and improve performance
- **🎨 Dual Interface**: Both command-line and graphical user interface options
- **✅ Portfolio Analysis**: Validates existing investments against your risk profile
- **📈 Personalized Recommendations**: Algorithm-based stock matching with detailed explanations


## 🚀 Quick Start

### Prerequisites

- OCaml 5.0+ and opam
- Alpha Vantage API key ([Get one free](https://www.alphavantage.co/support/#api-key))

### Installation

See [INSTALL.md](INSTALL.md) for detailed installation instructions.


## 📖 Usage

### 1. Refresh Stock Cache (Recommended)

```bash
# Refresh all default stocks (20 stocks, ~5 minutes)
dune exec -- FinalProject-refresh

# Or refresh specific stocks
dune exec -- FinalProject-refresh AAPL,MSFT,GOOGL
```

### 2. Run the Questionnaire

**CLI Version:**
```bash
dune exec -- FinalProject
```

**GUI Version:**
```bash
dune exec -- FinalProject gui
```

The questionnaire will:
1. Ask about your investment preferences
2. Calculate your personalized risk profile
3. Analyze your current investments (if any)
4. Provide stock recommendations matching your risk profile


## 🔧 API Integration

OptiFolio uses the [Alpha Vantage API](https://www.alphavantage.co/documentation/) for stock data:

- **Endpoint**: `TIME_SERIES_DAILY` function
- **Rate Limit**: 5 requests per minute (free tier)
- **Data Cached**: Stock analyses are cached locally to minimize API calls
- **Automatic Fetching**: Stocks not in cache are automatically fetched during questionnaire

### Supported Stock Symbols

The default portfolio includes 20 large-cap stocks:
AAPL, MSFT, NVDA, GOOGL, AMZN, META, TSLA, JPM, BAC, WMT, COST, HD, V, MA, UNH, PEP, KO, XOM, CVX, DIS

You can analyze any valid stock symbol by entering it during the questionnaire.

## 🧪 Testing

Run the test suite:

```bash
dune runtest
```

Tests cover financial analytics calculations including average, variance, standard deviation, return ratios, cumulative returns, volatility, max drawdown, and Sharpe ratio.

## 📊 How It Works

### Risk Profile Calculation

The system converts your questionnaire responses into a quantitative risk profile:

- **Base Risk**: Determined by your risk tolerance (Conservative: 0.2, Moderate: 0.5, Aggressive: 0.8)
- **Adjustments**: Modified by investment goal, experience, time horizon, and loss tolerance
- **Metrics**: Calculates target volatility, minimum Sharpe ratio, and drawdown tolerance

### Stock Scoring Algorithm

Each stock is scored based on:
- **Volatility Match** (30%): How close the stock's volatility is to your target
- **Sharpe Ratio** (30%): Risk-adjusted return performance
- **Drawdown Tolerance** (20%): Maximum decline within acceptable range
- **Return Score** (20%): Historical return performance

### Recommendation Generation

1. Load all cached stocks
2. Filter out stocks you already own
3. Score each stock against your risk profile
4. Sort by match score
5. Return top N recommendations (based on your portfolio size preference)


## 📝 Configuration

- **API Throttle**: 15 seconds between API calls (configurable in `lib/refresh.ml`)
- **Minimum Data Points**: 30 price points required for analysis
- **Cache Location**: `data/stock_cache.json`
- **Default Stocks**: Defined in `lib/refresh.ml`

## 👥 Authors

- **Daniel Zlotnick** (dz344)
- **Lulu Wang** (lw765)
- **Ani Jain** (aj655)
- **Priya Gokhale** (pg492)


---

**Made with ❤️ using OCaml**
