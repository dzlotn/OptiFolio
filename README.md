Daniel Zlotnick (dz344)
Lulu Wang (lw765)
Ani Jain (aj655)
Priya Gokhale (pg492)

## Getting the Stock API to work

We provide a small OCaml demo in `bin/api.ml` that fetches quote data for 20
large-cap tickers using the [Alpha Vantage Global Quote API](https://www.alphavantage.co/documentation/).
Follow the steps below to run it locally.

1. **Install OCaml dependencies (one time)**

   ```bash
   opam install cohttp-lwt-unix yojson lwt lwt_ppx lwt_ssl
   ```

   These packages give us HTTPS support plus JSON parsing.

2. **Set your Alpha Vantage key**

   Create a free API key at Alpha Vantage and export it before running the demo:

   ```bash
   export ALPHAVANTAGE_API_KEY=your_key_here
   ```

   Keeping the key in an environment variable avoids checking secrets into git.

3. **Run the demo executable**

   ```bash
   dune exec bin/api.exe
   ```

   - The program iterates over the 20-symbol portfolio and calls the `GLOBAL_QUOTE`
     endpoint per ticker (Alpha Vantage’s free tier allows only 5 requests per minute),
     so we throttle with a 15-second sleep between symbols. Expect the run to take
     roughly five minutes.
   - Successful output lists symbol, price, day change, change percentage, and volume
     for each ticker. Errors from Alpha Vantage (e.g., rate-limit “Note” messages)
     are surfaced directly in the terminal.

If you need to adjust the tickers or throttle behavior, edit `diversified_symbols`
and `throttle_seconds` near the top of `bin/api.ml`, then rerun the command above.
