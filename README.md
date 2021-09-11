# How to

## Build

If you want to have an optimised executable then run the following command

```bash
stack build
```

If you want to have faster compilation times at the expense of having less optimised executable then run this

```bash
stack build --fast
```

## Run

First you need to start the database. Make sure you have `docker` and `docker-compose` installed and the `docker` service is running. Then run the following command

```bash
docker-compose up
```

If you don't want `docker-compose` to occupy your terminal then you can run it detached

```bash
docker-compose up -d
```

Then, assuming you have built the project, you can run the executable

```bash
stack exec make-money
```

## Test

```bash
stack test --no-keep-going --fast --file-watch --test-arguments "--color"
```

`--no-keep-going` stops tests from running as soon as one fails, `--fast` improves compilation time by disabling optimisations, `--file-watch` rebuilds and retests the project when a tracked file changes, and `--test-arguments "--color"` makes HSpec (the used testing library) print tests output with colors

Add `--ghc-options -Wwarn` at the end of that if you want to temporarily ignore non-critical warnings. The project is configured in a way that turns warnings into errors, this flag turns them back into warnings.

# Money Maker Roadmap

## Init

First we need to build the foundation for the project so that we have something that works but doesn't make money. This includes things like
- Set up the Haskell project
- Set up the Python project
- Integrate Haskell with Python
- Integrate Coinbase Pro API

I think we can consider this stage done when we can get price data from Coinbase Pro, pass it to the Python code, and execute a trade on Coinbase Pro.

## MVP

At this stage we want to build something that has a backtested hard-coded strategy behind it. We want to be able to run the bot and have it make decisions and place trades.
- Set up everything to make backtesting fast and easy
- Create a trading strategy
- Add authentication/authorization
- TBD: Deploy to production

Consider this stage done when you can run the bot and have it make and execute trading decisions.

## Smart Bot

At this point we want to start making the trading strategy smarter by adding machine learning algorithms to it. We also want to build graphs, stats, etc, basically we will need some sort of an interface to understand the performance of the bot. We might want to start supporting multiple strategies at this point, in which case having an interface for managing those will be even more important. To support multiple strategies we will also need to implement more advanced fund management mechanisms.
- Start integrating machine learning algorithms into the trading strategies
- TBD: support multiple strategies within one bot
- Build bot analytics & strategy management interface

I think to consider this stage done, we want to have something running in production and making real-life trades.

## Mission complete

Consider this stage done when we have mansions lol.

# Useful links
- [Retrieving historical Binance data](https://medium.com/swlh/retrieving-full-historical-data-for-every-cryptocurrency-on-binance-bitmex-using-the-python-apis-27b47fd8137f)
- [Binance Websocket API documentation](https://github.com/binance/binance-spot-api-docs/blob/master/web-socket-streams.md#trade-streams)
